// Configuration for removing the ison from the microphone signal. See
// ISON_CANCELLER_SOURCE below.
const ISON_CANCELLER_CONFIG = {
    // Width (-3 dB, in Hz) of the notch around the ison frequency once it has
    // converged. Narrower preserves more of a voice singing at or near the
    // ison, but adapts more slowly (time constant of about 1 / (pi * bandwidth)
    // seconds). The bleed is a steady tone, so slow is fine.
    bandwidthHz: 1,
    // Wider notch used briefly after the ison changes, so the new tone is
    // removed quickly.
    acquireBandwidthHz: 10,
    acquireSeconds: 0.5,
    // Number of harmonics of the ison to remove (1 = fundamental only). Raise
    // this if speaker distortion makes harmonics of the ison bleed into the mic
    // (small laptop speakers often radiate the 2nd harmonic more than a low
    // fundamental). Each extra harmonic also notches a voice sung at that
    // multiple of the ison.
    harmonics: 1,
} as const;

/**
 * Source for an AudioWorklet that removes a sine tone of known frequency (the
 * ison, played by <chant-engine> through the speakers) from the microphone
 * signal, so that it doesn't confuse pitch detection.
 *
 * It's a two-weight LMS adaptive noise canceller. The reference signals are a
 * sine and cosine generated locally at the ison frequency; the weights learn the
 * amplitude and phase of the ison as picked up by the mic, and that estimate is
 * subtracted from the input:
 *
 *     y = w1*sin(wt) + w2*cos(wt)    // estimate of the ison at the mic
 *     e = x - y                      // output: mic signal without the ison
 *     w1 += mu*e*sin(wt); w2 += mu*e*cos(wt)
 *
 * Only the frequency is needed, not the playback signal, so this works even
 * though playback and capture are in different AudioContexts. The step size is
 * derived from a target notch bandwidth: mu = 2*pi*bandwidth / sampleRate. A
 wider bandwidth is used for a short time after the frequency changes.
 *
 * A frequency of 0 disables cancellation (the input passes through unchanged).
 * It is kept as a string because the worklet is loaded from a Blob URL, which
 * avoids a separate build/serving step for the worklet file.
 */
const ISON_CANCELLER_SOURCE = `
class IsonCanceller extends AudioWorkletProcessor {
    static get parameterDescriptors() {
        return [
            {
                name: "frequency",
                defaultValue: 0,
                minValue: 0,
                maxValue: 24000,
                automationRate: "k-rate",
            },
        ];
    }

    constructor(options) {
        super();
        const config = (options && options.processorOptions) || {};
        this.harmonics = config.harmonics || 1;
        this.muSteady = (2 * Math.PI * config.bandwidthHz) / sampleRate;
        this.muAcquire = (2 * Math.PI * config.acquireBandwidthHz) / sampleRate;
        this.acquireSamples = Math.round(config.acquireSeconds * sampleRate);
        this.acquireRemaining = 0;
        this.phase = 0;
        this.lastFrequency = 0;
        this.weights = new Float64Array(2 * this.harmonics);
        this.sines = new Float64Array(this.harmonics);
        this.cosines = new Float64Array(this.harmonics);
    }

    process(inputs, outputs, parameters) {
        const output = outputs[0];
        if (!output || output.length === 0) return true;
        const out = output[0];

        const input = inputs[0];
        const inp = input && input[0];
        if (!inp) {
            out.fill(0);
            return true;
        }

        const frequency = parameters.frequency[0];
        if (!(frequency > 0)) {
            out.set(inp);
            this.lastFrequency = 0;
            return true;
        }

        if (frequency !== this.lastFrequency) {
            // The learned amplitude/phase belongs to the previous tone.
            this.weights.fill(0);
            this.lastFrequency = frequency;
            this.acquireRemaining = this.acquireSamples;
        }

        const twoPi = 2 * Math.PI;
        const phaseStep = (twoPi * frequency) / sampleRate;
        const n = this.harmonics;
        const w = this.weights;
        const s = this.sines;
        const c = this.cosines;

        for (let i = 0; i < inp.length; i++) {
            this.phase += phaseStep;
            if (this.phase >= twoPi) this.phase -= twoPi;

            let estimate = 0;
            for (let k = 0; k < n; k++) {
                const angle = (k + 1) * this.phase;
                s[k] = Math.sin(angle);
                c[k] = Math.cos(angle);
                estimate += w[2 * k] * s[k] + w[2 * k + 1] * c[k];
            }

            const error = inp[i] - estimate;
            out[i] = error;

            let mu = this.muSteady;
            if (this.acquireRemaining > 0) {
                mu = this.muAcquire;
                this.acquireRemaining--;
            }
            for (let k = 0; k < n; k++) {
                w[2 * k] += mu * error * s[k];
                w[2 * k + 1] += mu * error * c[k];
            }
        }

        return true;
    }
}

registerProcessor("ison-canceller", IsonCanceller);
`;

class PitchTracker extends HTMLElement {
    private audioContext: AudioContext | null = null;
    private analyser: AnalyserNode | null = null;
    private source: MediaStreamAudioSourceNode | null = null;
    private canceller: AudioWorkletNode | null = null;
    // Frequency (Hz) of the ison currently sounding, or 0 if none.
    private isonFrequency: number = 0;
    private canvas: HTMLCanvasElement | null = null;
    private canvasContext: CanvasRenderingContext2D | null = null;
    private noteDisplay: HTMLDivElement | null = null;
    private stream: MediaStream | null = null;
    private animationFrameId: number | null = null;
    private noteAnimationFrameId: number | null = null;
    private smoothing: string = "sensitive";
    private renderVisualization: boolean = true;

    private shadow: ShadowRoot;

    constructor() {
        super();
        this.shadow = this.attachShadow({ mode: "open" });
        this.createBaseUI();
    }

    static get observedAttributes(): string[] {
        return ["smoothing", "render-visualization", "ison"];
    }

    attributeChangedCallback(
        name: string,
        oldValue: string | null,
        newValue: string | null,
    ): void {
        if (oldValue === newValue) return;

        if (name === "ison") {
            const frequency = newValue ? parseFloat(newValue) : 0;
            this.isonFrequency =
                Number.isFinite(frequency) && frequency > 0 ? frequency : 0;
            const param = this.canceller?.parameters.get("frequency");
            if (param) {
                param.value = this.isonFrequency;
            }
        } else if (name === "smoothing" && newValue) {
            if (newValue === "sensitive" || newValue === "smooth") {
                this.smoothing = newValue;
                // Reinitialize if we're already running
                if (this.audioContext) {
                    this.stopAudioProcessing();
                    this.init();
                }
            }
        } else if (name === "render-visualization") {
            const newRenderVisualization = newValue === "true";
            if (this.renderVisualization !== newRenderVisualization) {
                this.renderVisualization = newRenderVisualization;

                if (this.renderVisualization) {
                    // Enabling visualization
                    this.createVisualElements();
                    if (this.canvas) {
                        this.canvasContext = this.canvas.getContext("2d");
                        // Start visual rendering if audio is already initialized
                        if (this.analyser && this.audioContext) {
                            // Add slight delay to ensure canvas is properly attached to DOM
                            setTimeout(() => {
                                this.startVisualRendering();
                            }, 10);
                        }
                    }
                } else {
                    // Disabling visualization
                    this.stopVisualRendering();
                    this.createVisualElements(); // This will remove visual elements
                }
            }
        }
    }

    get renderVisualizationEnabled(): boolean {
        return this.renderVisualization;
    }

    connectedCallback() {
        // Process initial attributes before initializing
        this.processInitialAttributes();
        this.init();
    }

    private processInitialAttributes() {
        // Process renderVisualization attribute
        const renderVizAttr = this.getAttribute("render-visualization");
        if (renderVizAttr !== null) {
            this.renderVisualization = renderVizAttr === "true";
        }

        // Process smoothing attribute
        const smoothingAttr = this.getAttribute("smoothing");
        if (
            smoothingAttr &&
            (smoothingAttr === "sensitive" || smoothingAttr === "smooth")
        ) {
            this.smoothing = smoothingAttr;
        }
    }

    disconnectedCallback() {
        this.stopAudioProcessing();
    }

    private createBaseUI() {
        // Add styles
        const style = document.createElement("style");
        style.textContent = `
            :host {
                display: block;
                max-width: 800px;
                margin: 0 auto;
                padding: 20px;
            }
            canvas {
                width: 100%;
                height: 200px;
                border: 1px solid #ccc;
                margin: 20px 0;
            }
            #note {
                font-size: 24px;
                font-weight: bold;
                text-align: center;
                margin: 20px 0;
                min-height: 36px;
            }
        `;

        this.shadow.appendChild(style);
    }

    private createVisualElements() {
        // Remove existing visual elements
        const existingNote = this.shadow.querySelector("#note");
        const existingCanvas = this.shadow.querySelector("canvas");
        if (existingNote) existingNote.remove();
        if (existingCanvas) existingCanvas.remove();

        if (this.renderVisualization) {
            // Create note display
            this.noteDisplay = document.createElement("div");
            this.noteDisplay.id = "note";

            // Create canvas
            this.canvas = document.createElement("canvas");
            this.canvas.width = 800;
            this.canvas.height = 200;

            // Append visual elements to shadow DOM
            this.shadow.appendChild(this.noteDisplay);
            this.shadow.appendChild(this.canvas);
        } else {
            this.noteDisplay = null;
            this.canvas = null;
            this.canvasContext = null;
        }
    }

    private dispatchPitchDetected = (pitch: number | null) => {
        // Create and dispatch a custom event with the detected pitch
        const event = new CustomEvent("pitchDetected", {
            bubbles: true,
            composed: true, // Allows the event to cross the shadow DOM boundary
            detail: {
                pitch: pitch,
                timestamp: Date.now(),
            },
        });

        this.dispatchEvent(event);
    };

    /*
The MIT License (MIT)
Copyright (c) 2014 Chris Wilson
Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:
The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.
THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

Note: autoCorrelate comes from https://github.com/cwilso/PitchDetect/pull/23
with the above license.

source: https://alexanderell.is/posts/tuner/tuner.js
https://alexanderell.is/posts/tuner/

*/

    private init = async () => {
        try {
            // If already initialized, clean up first
            if (this.audioContext) {
                this.stopAudioProcessing();
            }

            // Create visual elements based on current renderVisualization setting
            this.createVisualElements();

            // Create audio context
            const audioContext = new (window.AudioContext ||
                (window as any).webkitAudioContext)();
            this.audioContext = audioContext;
            this.analyser = this.audioContext.createAnalyser();
            this.analyser.minDecibels = -100;
            this.analyser.maxDecibels = -10;
            this.analyser.smoothingTimeConstant = 0.85;

            if (!navigator?.mediaDevices?.getUserMedia) {
                if (this.noteDisplay) {
                    this.noteDisplay.innerText =
                        "Error: Your browser doesn't support audio input.";
                }
                console.error("getUserMedia not supported");
                return;
            }

            try {
                // Browser mic processing is disabled on purpose:
                // - In Chrome, `audio: true` enables echo cancellation, which
                //   taps the page's audio output and produces audible clicks
                //   in other playing audio (e.g. the ison) whenever the mic
                //   stream is opened or closed.
                // - Noise suppression and AGC alter the voice signal that the
                //   autocorrelation pitch detector depends on.
                const constraints = {
                    audio: {
                        echoCancellation: false,
                        noiseSuppression: false,
                        autoGainControl: false,
                    },
                };
                // Load the ison canceller first so the graph is complete when
                // the mic opens. If it can't be loaded, the mic is analyzed
                // directly (the ison may then bleed into detection).
                const canceller = await this.createIsonCanceller(audioContext);

                const stream =
                    await navigator.mediaDevices.getUserMedia(constraints);

                // The element may have been disconnected, or re-initialized,
                // while waiting on the awaits above.
                if (this.audioContext !== audioContext) {
                    stream.getTracks().forEach((track) => track.stop());
                    return;
                }
                this.stream = stream;

                // Initialize the SourceNode
                this.source = audioContext.createMediaStreamSource(stream);

                // mic -> (ison canceller) -> analyser
                if (canceller) {
                    this.canceller = canceller;
                    this.source.connect(canceller);
                    canceller.connect(this.analyser);
                } else {
                    this.source.connect(this.analyser);
                }

                // Initialize canvas context only if renderVisualization is true
                if (this.canvas && this.renderVisualization) {
                    this.canvasContext = this.canvas.getContext("2d");
                }

                // Always start pitch detection, regardless of visualization
                this.visualize();
            } catch (mediaError) {
                if (this.noteDisplay) {
                    if ((mediaError as Error).name === "NotAllowedError") {
                        this.noteDisplay.innerText =
                            "Microphone access denied. Please grant permission to use the pitch detector.";
                    } else if ((mediaError as Error).name === "NotFoundError") {
                        this.noteDisplay.innerText =
                            "No microphone detected. Please connect a microphone and try again.";
                    } else {
                        this.noteDisplay.innerText =
                            "Error accessing microphone: " +
                            (mediaError as Error).message;
                    }
                }
                console.error("Media error:", mediaError);
            }
        } catch (err) {
            if (this.noteDisplay) {
                this.noteDisplay.innerText =
                    "Error initializing audio system. Please try again.";
            }
            console.error("Error initializing audio:", err);
        }
    };

    private stopAudioProcessing() {
        // Cancel any ongoing animation frames
        this.stopVisualRendering();

        if (this.noteAnimationFrameId !== null) {
            cancelAnimationFrame(this.noteAnimationFrameId);
            this.noteAnimationFrameId = null;
        }

        // Close the audio context
        if (this.audioContext) {
            if (this.audioContext.state !== "closed") {
                this.audioContext.close();
            }
            this.audioContext = null;
        }

        // Stop the media stream
        if (this.stream) {
            this.stream.getTracks().forEach((track) => track.stop());
            this.stream = null;
        }

        // Clean up references
        this.source = null;
        this.canceller = null;
        this.analyser = null;
    }

    /**
     * Creates the worklet node that removes the ison from the mic signal, or
     * returns null if AudioWorklet is unavailable or fails to load.
     */
    private async createIsonCanceller(
        audioContext: AudioContext,
    ): Promise<AudioWorkletNode | null> {
        if (!audioContext.audioWorklet || typeof AudioWorkletNode === "undefined") {
            return null;
        }

        const moduleUrl = URL.createObjectURL(
            new Blob([ISON_CANCELLER_SOURCE], { type: "text/javascript" }),
        );
        try {
            await audioContext.audioWorklet.addModule(moduleUrl);
            return new AudioWorkletNode(audioContext, "ison-canceller", {
                numberOfInputs: 1,
                numberOfOutputs: 1,
                outputChannelCount: [1],
                channelCount: 1,
                channelCountMode: "explicit",
                parameterData: { frequency: this.isonFrequency },
                processorOptions: {
                    bandwidthHz: ISON_CANCELLER_CONFIG.bandwidthHz,
                    acquireBandwidthHz: ISON_CANCELLER_CONFIG.acquireBandwidthHz,
                    acquireSeconds: ISON_CANCELLER_CONFIG.acquireSeconds,
                    harmonics: ISON_CANCELLER_CONFIG.harmonics,
                },
            });
        } catch (error) {
            console.error("Unable to load the ison canceller:", error);
            return null;
        } finally {
            URL.revokeObjectURL(moduleUrl);
        }
    }

    private visualize() {
        if (!this.analyser || !this.audioContext) {
            return;
        }

        // Start pitch detection (always runs)
        this.startPitchDetection();

        // Start visual rendering (only if renderVisualization is true)
        if (this.renderVisualization && this.canvas && this.canvasContext) {
            this.startVisualRendering();
        }
    }

    private startPitchDetection() {
        let previousValueToDisplay: number | string = 0;
        let smoothingCount = 0;
        let smoothingThreshold = 5;
        let smoothingCountThreshold = 5;

        const detectPitch = () => {
            this.noteAnimationFrameId = requestAnimationFrame(detectPitch);

            if (!this.analyser || !this.audioContext) return;

            const bufferLength = this.analyser.fftSize;
            const buffer = new Float32Array(bufferLength);
            this.analyser.getFloatTimeDomainData(buffer);
            const autoCorrelateValue = this.autoCorrelate(
                buffer,
                this.audioContext.sampleRate,
            );

            // Use full precision for processing
            let valueToDisplay: number | string = autoCorrelateValue;
            const smoothingValue = this.smoothing;

            if (autoCorrelateValue === -1) {
                // Dispatch null when no pitch is detected (too quiet)
                this.dispatchPitchDetected(null);
                this.updateNoteDisplay("Too quiet...");
                return;
            }

            // Set smoothing parameters based on current setting
            if (smoothingValue === "sensitive") {
                smoothingThreshold = 10;
                smoothingCountThreshold = 5;
            } else if (smoothingValue === "smooth") {
                smoothingThreshold = 5;
                smoothingCountThreshold = 10;
            }

            const noteIsSimilarEnough = () => {
                if (
                    typeof valueToDisplay === "number" &&
                    typeof previousValueToDisplay === "number"
                ) {
                    return (
                        Math.abs(valueToDisplay - previousValueToDisplay) <
                        smoothingThreshold
                    );
                } else {
                    return valueToDisplay === previousValueToDisplay;
                }
            };

            // Apply smoothing logic
            if (noteIsSimilarEnough()) {
                if (smoothingCount < smoothingCountThreshold) {
                    smoothingCount++;
                    return;
                } else {
                    previousValueToDisplay = valueToDisplay;
                    smoothingCount = 0;
                }
            } else {
                previousValueToDisplay = valueToDisplay;
                smoothingCount = 0;
                return;
            }

            // Process the final detected pitch
            if (typeof valueToDisplay === "number") {
                // Dispatch pitch detected event when we have a valid frequency
                this.dispatchPitchDetected(autoCorrelateValue);

                // Update visual display
                const formattedValue = valueToDisplay.toFixed(2) + " Hz";
                this.updateNoteDisplay(formattedValue);
            } else {
                // Dispatch null when no pitch is detected
                this.dispatchPitchDetected(null);
                this.updateNoteDisplay(valueToDisplay);
            }
        };

        detectPitch();
    }

    private updateNoteDisplay(text: string | number) {
        // Only update display if renderVisualization is enabled and noteDisplay exists
        if (this.renderVisualization && this.noteDisplay) {
            this.noteDisplay.innerText = String(text);
        }
    }

    private stopVisualRendering() {
        if (this.animationFrameId !== null) {
            cancelAnimationFrame(this.animationFrameId);
            this.animationFrameId = null;
        }
    }

    private startVisualRendering() {
        if (!this.canvas || !this.canvasContext) {
            return;
        }

        // Check canvas dimensions
        const rect = this.canvas.getBoundingClientRect();

        if (rect.width === 0 || rect.height === 0) {
            setTimeout(() => {
                this.startVisualRendering();
            }, 50);
            return;
        }

        // Stop any existing visual rendering first
        this.stopVisualRendering();

        // Get current canvas dimensions
        const WIDTH = this.canvas.width;
        const HEIGHT = this.canvas.height;

        const drawFrequency = () => {
            if (!this.analyser || !this.canvasContext) return;

            const bufferLengthAlt = this.analyser.frequencyBinCount;
            const dataArrayAlt = new Uint8Array(bufferLengthAlt);

            this.canvasContext.clearRect(0, 0, WIDTH, HEIGHT);

            const drawAlt = () => {
                this.animationFrameId = requestAnimationFrame(drawAlt);

                if (!this.analyser || !this.canvasContext) return;

                this.analyser.getByteFrequencyData(dataArrayAlt);

                this.canvasContext.fillStyle = "rgb(0, 0, 0)";
                this.canvasContext.fillRect(0, 0, WIDTH, HEIGHT);

                const barWidth = (WIDTH / bufferLengthAlt) * 2.5;
                let barHeight;
                let x = 0;

                for (let i = 0; i < bufferLengthAlt; i++) {
                    barHeight = dataArrayAlt[i];

                    this.canvasContext.fillStyle =
                        "rgb(" + (barHeight + 100) + ",50,50)";
                    this.canvasContext.fillRect(
                        x,
                        HEIGHT - barHeight / 2,
                        barWidth,
                        barHeight / 2,
                    );

                    x += barWidth + 1;
                }
            };

            drawAlt();
        };

        drawFrequency();
    }

    private autoCorrelate(buffer: Float32Array, sampleRate: number): number {
        // Perform a quick root-mean-square to see if we have enough signal
        const SIZE = buffer.length;
        let sumOfSquares = 0;
        for (let i = 0; i < SIZE; i++) {
            const val = buffer[i];
            sumOfSquares += val * val;
        }
        const rootMeanSquare = Math.sqrt(sumOfSquares / SIZE);
        if (rootMeanSquare < 0.01) {
            return -1;
        }

        // Find a range in the buffer where the values are below a given threshold.
        let r1 = 0;
        let r2 = SIZE - 1;
        const threshold = 0.2;

        // Walk up for r1
        for (let i = 0; i < SIZE / 2; i++) {
            if (Math.abs(buffer[i]) < threshold) {
                r1 = i;
                break;
            }
        }

        // Walk down for r2
        for (let i = 1; i < SIZE / 2; i++) {
            if (Math.abs(buffer[SIZE - i]) < threshold) {
                r2 = SIZE - i;
                break;
            }
        }

        // Trim the buffer to these ranges and update SIZE.
        buffer = buffer.slice(r1, r2);
        const trimmedSize = buffer.length;

        // Create a new array of the sums of offsets to do the autocorrelation
        const c = new Array(trimmedSize).fill(0);
        // For each potential offset, calculate the sum of each buffer value times its offset value
        for (let i = 0; i < trimmedSize; i++) {
            for (let j = 0; j < trimmedSize - i; j++) {
                c[i] = c[i] + buffer[j] * buffer[j + i];
            }
        }

        // Find the last index where that value is greater than the next one (the dip)
        let d = 0;
        while (c[d] > c[d + 1]) {
            d++;
        }

        // Iterate from that index through the end and find the maximum sum
        let maxValue = -1;
        let maxIndex = -1;
        for (let i = d; i < trimmedSize; i++) {
            if (c[i] > maxValue) {
                maxValue = c[i];
                maxIndex = i;
            }
        }

        let T0 = maxIndex;

        // Parabolic interpolation for better precision
        const x1 = c[T0 - 1];
        const x2 = c[T0];
        const x3 = c[T0 + 1];

        const a = (x1 + x3 - 2 * x2) / 2;
        const b = (x3 - x1) / 2;
        if (a) {
            T0 = T0 - b / (2 * a);
        }

        return sampleRate / T0;
    }
}

window.customElements.define("pitch-tracker", PitchTracker);
