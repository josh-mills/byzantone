class PitchTracker extends HTMLElement {
    private audioContext: AudioContext | null = null;
    private analyser: AnalyserNode | null = null;
    private source: MediaStreamAudioSourceNode | null = null;
    private canvas: HTMLCanvasElement | null = null;
    private canvasContext: CanvasRenderingContext2D | null = null;
    private noteDisplay: HTMLDivElement | null = null;
    private stream: MediaStream | null = null;
    private animationFrameId: number | null = null;
    private noteAnimationFrameId: number | null = null;
    private smoothing: string = "sensitive";

    private shadow: ShadowRoot;
    private resizeObserver: ResizeObserver | null = null;

    constructor() {
        super();
        console.log("PitchTracker constructor called");
        this.shadow = this.attachShadow({ mode: "open" });
        this.createUI();
    }

    static get observedAttributes(): string[] {
        return ["smoothing"];
    }

    attributeChangedCallback(
        name: string,
        oldValue: string | null,
        newValue: string | null,
    ): void {
        if (oldValue === newValue) return;

        if (name === "smoothing" && newValue) {
            if (newValue === "sensitive" || newValue === "smooth") {
                this.smoothing = newValue;
                // Reinitialize if we're already running
                if (this.audioContext) {
                    this.stopAudioProcessing();
                    this.init();
                }
            }
        }
    }

    connectedCallback() {
        this.init();
    }

    disconnectedCallback() {
        this.stopAudioProcessing();

        // Disconnect the resize observer
        if (this.resizeObserver) {
            this.resizeObserver.disconnect();
            this.resizeObserver = null;
        }
    }

    private createUI() {
        // Add styles
        const style = document.createElement("style");
        style.textContent = `
            :host {
                display: block;
                max-width: 800px;
                margin: 0 auto;
                padding: 20px;
            }
            .controls {
                margin: 0 0 20px 0;
            }
            .control-group {
                margin-bottom: 15px;
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
            .controls {
                margin-top: 0;
            }
        `;

        // Create note display
        this.noteDisplay = document.createElement("div");
        this.noteDisplay.id = "note";

        // Create canvas
        this.canvas = document.createElement("canvas");
        this.canvas.className = "visualizer";
        this.canvas.width = 800;
        this.canvas.height = 200;

        // Append everything to shadow DOM
        this.shadow.appendChild(style);
        this.shadow.appendChild(this.noteDisplay);
        this.shadow.appendChild(this.canvas);
    }

    private dispatchPitchDetected = (pitch: number | null) => {
        // Create and dispatch a custom event with the detected pitch
        const event = new CustomEvent("pitchDetected", {
            bubbles: true,
            composed: true, // Allows the event to cross the shadow DOM boundary
            detail: { pitch: pitch },
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

            // Create audio context
            this.audioContext = new (window.AudioContext ||
                (window as any).webkitAudioContext)();
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
                const constraints = { audio: true };
                this.stream =
                    await navigator.mediaDevices.getUserMedia(constraints);

                // Initialize the SourceNode
                this.source = this.audioContext.createMediaStreamSource(
                    this.stream,
                );
                // Connect the source node to the analyzer
                this.source.connect(this.analyser);

                // Initialize canvas context
                if (this.canvas) {
                    this.canvasContext = this.canvas.getContext("2d");

                    // Set up resize observer for the canvas
                    this.resizeObserver = new ResizeObserver(() => {
                        if (this.canvas) {
                            // Update canvas dimensions when container resizes
                            const rect = this.canvas.getBoundingClientRect();
                            this.canvas.width = rect.width;
                            this.canvas.height = rect.height;
                        }
                    });
                    this.resizeObserver.observe(this.canvas);

                    this.visualize();
                }
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
        if (this.animationFrameId !== null) {
            cancelAnimationFrame(this.animationFrameId);
            this.animationFrameId = null;
        }

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
        this.analyser = null;
    }

    private visualize() {
        if (
            !this.canvas ||
            !this.canvasContext ||
            !this.analyser ||
            !this.audioContext
        ) {
            return;
        }

        // Get current canvas dimensions each time we draw
        const WIDTH = this.canvas.width;
        const HEIGHT = this.canvas.height;

        let previousValueToDisplay: number | string = 0;
        let smoothingCount = 0;
        let smoothingThreshold = 5;
        let smoothingCountThreshold = 5;

        const drawNote = () => {
            this.noteAnimationFrameId = requestAnimationFrame(drawNote);

            if (!this.analyser || !this.audioContext) return;

            const bufferLength = this.analyser.fftSize;
            const buffer = new Float32Array(bufferLength);
            this.analyser.getFloatTimeDomainData(buffer);
            const autoCorrelateValue = this.autoCorrelate(
                buffer,
                this.audioContext.sampleRate,
            );

            // Use full precision
            let valueToDisplay: number | string = autoCorrelateValue;

            const smoothingValue = this.smoothing;

            if (autoCorrelateValue === -1) {
                if (this.noteDisplay) {
                    this.noteDisplay.innerText = "Too quiet...";
                }
                // Dispatch null when no pitch is detected (too quiet)
                this.dispatchPitchDetected(null);
                return;
            }

            if (smoothingValue === "sensitive") {
                smoothingThreshold = 10;
                smoothingCountThreshold = 5;
            } else if (smoothingValue === "smooth") {
                smoothingThreshold = 5;
                smoothingCountThreshold = 10;
            }

            const noteIsSimilarEnough = () => {
                // Check threshold for number, or just difference for notes.
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

            // Check if this value has been within the given range for n iterations
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

            if (typeof valueToDisplay === "number") {
                // Format with 2 decimal places for better readability while keeping precision
                valueToDisplay = valueToDisplay.toFixed(2) + " Hz";

                // Dispatch pitch detected event when we have a valid frequency
                this.dispatchPitchDetected(autoCorrelateValue);
            } else {
                // Dispatch null when no pitch is detected
                this.dispatchPitchDetected(null);
            }

            if (this.noteDisplay) {
                this.noteDisplay.innerText = valueToDisplay;
            }
        };

        // Draw frequency visualization
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

        drawNote();
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
