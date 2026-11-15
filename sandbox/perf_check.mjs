// Rough CPU cost comparison (run with node after `npx tsc`):
//   - the ison canceller worklet, per second of audio
//   - PitchTracker.autoCorrelate, which runs once per animation frame
// Absolute numbers are for the machine running this, not for phones; use the
// ratio between the two.
import fs from "node:fs";
import vm from "node:vm";

const compiled = fs.readFileSync("public/dist/src/pitch_tracker.js", "utf8");

// --- worklet ---
const workletSrc = compiled.match(
    /const ISON_CANCELLER_SOURCE = `([\s\S]*?)`;/,
)[1];
const SR = 48000;

function makeWorklet(harmonics) {
    let Processor;
    vm.runInNewContext(workletSrc, {
        sampleRate: SR,
        AudioWorkletProcessor: class {},
        registerProcessor: (_, cls) => (Processor = cls),
        Math,
        Float64Array,
    });
    return new Processor({
        processorOptions: {
            bandwidthHz: 1,
            acquireBandwidthHz: 10,
            acquireSeconds: 0.5,
            harmonics,
        },
    });
}

function benchWorklet(label, harmonics, frequency) {
    const p = makeWorklet(harmonics);
    const input = new Float32Array(128);
    for (let i = 0; i < 128; i++) input[i] = 0.1 * Math.sin(i * 0.3);
    const output = new Float32Array(128);
    const quantaPerSecond = SR / 128;
    const seconds = 20;
    const params = { frequency: [frequency] };
    // warm up
    for (let i = 0; i < 20000; i++) p.process([[input]], [[output]], params);
    const start = performance.now();
    const quanta = Math.round(quantaPerSecond * seconds);
    for (let i = 0; i < quanta; i++) p.process([[input]], [[output]], params);
    const ms = performance.now() - start;
    const perAudioSecond = ms / seconds;
    console.log(
        `worklet ${label}: ${perAudioSecond.toFixed(2)} ms CPU per second of audio ` +
            `(${((perAudioSecond / 1000) * 100).toFixed(2)}% of one core); ` +
            `${((ms / quanta) * 1000).toFixed(1)} us per 128-frame quantum (budget ${((128 / SR) * 1e6).toFixed(0)} us)`,
    );
}

benchWorklet("passthrough (no ison)  ", 1, 0);
benchWorklet("filter, 1 harmonic     ", 1, 220);
benchWorklet("filter, 2 harmonics    ", 2, 220);
benchWorklet("filter, 3 harmonics    ", 3, 220);

// --- autoCorrelate ---
const PitchTracker = vm.runInNewContext(
    compiled + "\n;PitchTracker",
    {
        HTMLElement: class {
            attachShadow() {
                return {};
            }
        },
        window: { customElements: { define() {} } },
        document: {},
        console,
    },
);
const autoCorrelate = PitchTracker.prototype.autoCorrelate;

function benchAutoCorrelate(label, amplitude, size) {
    const buffer = new Float32Array(size);
    for (let i = 0; i < size; i++) {
        const t = i / SR;
        buffer[i] =
            amplitude *
            (Math.sin(2 * Math.PI * 330 * t) +
                0.5 * Math.sin(2 * Math.PI * 660 * t) +
                0.25 * Math.sin(2 * Math.PI * 990 * t));
    }
    const calls = 600; // 10 s of frames at 60 fps
    for (let i = 0; i < 60; i++) autoCorrelate.call({}, buffer.slice(), SR);
    const start = performance.now();
    for (let i = 0; i < calls; i++) {
        autoCorrelate.call({}, new Float32Array(buffer), SR);
    }
    const ms = performance.now() - start;
    const perCall = ms / calls;
    console.log(
        `autoCorrelate ${label}: ${perCall.toFixed(2)} ms per call; ` +
            `at 60 fps = ${(perCall * 60).toFixed(0)} ms CPU per second ` +
            `(${(((perCall * 60) / 1000) * 100).toFixed(1)}% of one core; ` +
            `${((perCall / (1000 / 60)) * 100).toFixed(0)}% of a 16.7 ms frame)`,
    );
}

benchAutoCorrelate("amp 0.05, 2048 samples", 0.05, 2048);
benchAutoCorrelate("amp 0.5,  2048 samples", 0.5, 2048);
