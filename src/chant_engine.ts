const crossFadeSeconds = 0.2;
const crossFadeMilliseconds = crossFadeSeconds * 1000;

const localDev = true;
const debugging = false;
function devLog(x: any) {
    if (localDev) {
        if (debugging) {
            debugger;
        }
        console.log(x);
    }
}

class ChantEngine extends HTMLElement {
    private audioContext: AudioContext;
    private mainGainNode: GainNode;
    private melos: OscillatorNode | undefined;

    static maxFrequency: number = 18000;
    static minFrequency: number = 32;

    constructor() {
        super();

        this.audioContext = new AudioContext();

        this.mainGainNode = this.audioContext.createGain();
        this.mainGainNode.connect(this.audioContext.destination);

        const gainAttribute = this.getAttribute("gain");
        const gain = gainAttribute ? parseFloat(gainAttribute) : 0.3;
        this.mainGainNode.gain.value = gain;
    }

    connectedCallback() {
        devLog("connecting chant engine element...");
    }

    disconnectedCallback() {
        devLog("disconnecting chant engine...");
        this.stopTone(this.melos);
        this.audioContext.close();
    }

    static get observedAttributes(): string[] {
        return ["gain", "ison", "melos"];
    }

    attributeChangedCallback(
        name: string,
        oldValue: string,
        newValue: string,
    ): void {
        switch (name) {
            case "gain":
                this.mainGainNode.gain.value = Number(newValue);
                break;

            case "melos":
                const oldFreq = parseFloat(oldValue);
                const newFreq = parseFloat(newValue);
                if (this.melos) {
                    // TODO: if oldFreq == newFreq, some sort of re-articulation
                    if (newFreq) {
                        devLog(`changing frequency to ${newFreq}`);
                        this.melos.frequency.value = newFreq;
                    } else {
                        devLog("stopping tone");
                        this.stopTone(this.melos);
                        this.melos = undefined;
                    }
                } else {
                    devLog("playing tone");
                    this.melos = this.playTone(newFreq);
                }
                break;

            // case "melos":
            //     if (this.melos) {
            //         this.melos.stop();
            //     }
            //     this.melos = this.playTone(newValue);
            //     break;
        }
    }

    /** Instantiate and start an oscillator node for the given frequency.
     * Returns `null` if frequency is out of the range [32, 18000].
     *
     * @param {number} freq - Frequency to be played.
     * @returns {OscillatorNode | null}
     */
    private playTone(freq: number): OscillatorNode | undefined {
        if (
            !Number.isFinite(freq) ||
            freq < ChantEngine.minFrequency ||
            freq > ChantEngine.maxFrequency
        )
            return undefined;

        devLog(`playing ${freq}`);

        const osc = this.audioContext.createOscillator();
        osc.frequency.value = freq;
        osc.type = "sine";

        const now = this.audioContext.currentTime;
        const gain = this.audioContext.createGain();

        // consider also exponentialRampToValueAtTime
        gain.gain.setValueAtTime(0.01, now);
        gain.gain.exponentialRampToValueAtTime(
            this.mainGainNode.gain.value,
            now + crossFadeSeconds,
        );

        osc.connect(gain);

        gain.connect(this.mainGainNode);

        osc.start();
        devLog(osc);
        return osc;
    }

    private stopTone(osc: OscillatorNode | undefined): void {
        if (osc == null) return;
        devLog(`stopping ${osc.frequency.value}`);

        const now = this.audioContext.currentTime;
        // const gain = this.audioContext.createGain();

        // gain.gain.setValueAtTime(this.mainGainNode.gain.value, now);
        // gain.gain.linearRampToValueAtTime(0.01, now + crossFadeSeconds);

        // osc.connect(gain);
        // osc.connect(this.audioContext.destination);
        // gain.connect(this.mainGainNode);

        osc.stop(now + crossFadeSeconds);

        // setTimeout(() => {
        //     devLog("stopping now?")
        //     osc.stop();
        // }, crossFadeMilliseconds + 1)
        // setTimeout
        osc.disconnect();
        // gain.disconnect();
        osc.connect(this.mainGainNode);

        // consider also an equal-power crossfade technique for when a tone changes rather than just stops
    }
}

window.customElements.define("chant-engine", ChantEngine);
