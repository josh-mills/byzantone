// Audio configuration constants
const AUDIO_CONFIG = {
    crossFadeSeconds: 0.2,
    defaultGain: 0.3,
    frequency: {
        min: 32,
        max: 18000,
    },
} as const;

// Development constants
const DEV_CONFIG = {
    localDev: false,
    debugging: false,
} as const;

function devLog(message: unknown): void {
    if (DEV_CONFIG.localDev) {
        if (DEV_CONFIG.debugging) {
            debugger;
        }
        console.log(message);
    }
}

interface ToneNode {
    oscillator: OscillatorNode;
    gainNode: GainNode;
}

class ChantEngine extends HTMLElement {
    // These are undefined if the browser refused to create an AudioContext
    // (e.g. NotSupportedError when a hardware context limit is reached). In
    // that case the engine is a silent no-op.
    private readonly audioContext?: AudioContext;
    private readonly mainGainNode?: GainNode;
    private ison?: ToneNode;
    private melos?: ToneNode;

    static get maxFrequency(): number {
        return AUDIO_CONFIG.frequency.max;
    }

    static get minFrequency(): number {
        return AUDIO_CONFIG.frequency.min;
    }

    constructor() {
        super();
        try {
            this.audioContext = new AudioContext();
        } catch (error) {
            console.error("chant-engine: unable to create AudioContext", error);
            return;
        }
        this.mainGainNode = this.audioContext.createGain();
        this.mainGainNode.connect(this.audioContext.destination);

        const gainAttribute = this.getAttribute("gain");
        const gain = gainAttribute
            ? parseFloat(gainAttribute)
            : AUDIO_CONFIG.defaultGain;
        this.mainGainNode.gain.value = gain;
    }

    connectedCallback(): void {
        devLog("connecting chant engine element...");
    }

    disconnectedCallback(): void {
        devLog("disconnecting chant engine...");
        this.stopTone(this.ison);
        this.stopTone(this.melos);
        this.audioContext?.close();
    }

    static get observedAttributes(): string[] {
        return ["gain", "ison", "melos"];
    }

    attributeChangedCallback(
        name: string,
        oldValue: string | null,
        newValue: string | null,
    ): void {
        if (oldValue === newValue) return;

        switch (name) {
            case "gain":
                if (this.mainGainNode && newValue !== null) {
                    this.mainGainNode.gain.value = Number(newValue);
                }
                break;

            case "ison":
                this.handleFrequencyChange(
                    oldValue,
                    newValue,
                    this.ison,
                    (tone) => (this.ison = tone),
                );
                break;

            case "melos":
                this.handleFrequencyChange(
                    oldValue,
                    newValue,
                    this.melos,
                    (tone) => (this.melos = tone),
                );
                break;
        }
    }

    private handleFrequencyChange(
        oldValue: string | null,
        newValue: string | null,
        currentTone: ToneNode | undefined,
        setTone: (tone: ToneNode | undefined) => void,
    ): void {
        const newFreq = newValue ? parseFloat(newValue) : undefined;

        if (currentTone) {
            if (newFreq) {
                devLog(`changing frequency to ${newFreq}`);
                currentTone.oscillator.frequency.value = newFreq;
            } else {
                devLog("stopping tone");
                this.stopTone(currentTone);
                setTone(undefined);
            }
        } else if (newFreq) {
            devLog("playing new tone");
            setTone(this.playTone(newFreq));
        }
    }

    private playTone(freq: number): ToneNode | undefined {
        if (!this.audioContext || !this.mainGainNode) return undefined;

        if (
            !Number.isFinite(freq) ||
            freq < ChantEngine.minFrequency ||
            freq > ChantEngine.maxFrequency
        ) {
            return undefined;
        }

        devLog(`playing ${freq}`);

        // The engine may be mounted before any user gesture (it stays mounted
        // across audio modes), in which case the context starts suspended.
        // Tones are only started in response to user input, so resuming here is
        // permitted by autoplay policies.
        if (this.audioContext.state === "suspended") {
            void this.audioContext.resume();
        }

        const oscillator = this.audioContext.createOscillator();
        const gainNode = this.audioContext.createGain();
        const now = this.audioContext.currentTime;

        oscillator.frequency.value = freq;
        oscillator.type = "sine";

        // Start with near-zero gain and ramp up for smooth start
        gainNode.gain.setValueAtTime(0.001, now);
        gainNode.gain.exponentialRampToValueAtTime(
            this.mainGainNode.gain.value,
            now + AUDIO_CONFIG.crossFadeSeconds,
        );

        oscillator.connect(gainNode);
        gainNode.connect(this.mainGainNode);
        oscillator.start();

        return { oscillator, gainNode };
    }

    private stopTone(tone?: ToneNode): void {
        if (!tone || !this.audioContext) return;

        const { oscillator, gainNode } = tone;
        const now = this.audioContext.currentTime;

        // Implement proper cross-fade out
        gainNode.gain.setValueAtTime(gainNode.gain.value, now);
        gainNode.gain.exponentialRampToValueAtTime(
            0.001,
            now + AUDIO_CONFIG.crossFadeSeconds,
        );

        // Schedule the complete stop and cleanup
        oscillator.stop(now + AUDIO_CONFIG.crossFadeSeconds);

        setTimeout(() => {
            oscillator.disconnect();
            gainNode.disconnect();
        }, AUDIO_CONFIG.crossFadeSeconds * 1000);
    }
}

window.customElements.define("chant-engine", ChantEngine);
