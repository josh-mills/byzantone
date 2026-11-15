// Offline numerical check of the ison canceller worklet (run with node).
// Extracts the worklet source from the compiled pitch_tracker.js, runs it
// against synthetic mic signals, and reports residual power at the ison and
// voice frequencies.
import fs from "node:fs";
import vm from "node:vm";

const compiled = fs.readFileSync("public/dist/src/pitch_tracker.js", "utf8");
const match = compiled.match(/const ISON_CANCELLER_SOURCE = `([\s\S]*?)`;/);
if (!match) throw new Error("worklet source not found");
const config = {
    bandwidthHz: 1,
    acquireBandwidthHz: 10,
    acquireSeconds: 0.5,
    harmonics: 1,
};

const SR = 48000;

function makeProcessor(overrides = {}) {
    let Processor;
    const sandbox = {
        sampleRate: SR,
        AudioWorkletProcessor: class {},
        registerProcessor: (_, cls) => (Processor = cls),
        Math,
        Float64Array,
    };
    vm.runInNewContext(match[1], sandbox);
    return new Processor({ processorOptions: { ...config, ...overrides } });
}

// Power of x at frequency f (single-bin DFT) over [from, to) samples.
function powerAt(x, f, from, to) {
    let re = 0;
    let im = 0;
    for (let n = from; n < to; n++) {
        const a = (2 * Math.PI * f * n) / SR;
        re += x[n] * Math.cos(a);
        im -= x[n] * Math.sin(a);
    }
    const amp = (2 * Math.sqrt(re * re + im * im)) / (to - from);
    return amp;
}

function run({ seconds, mic, frequency }) {
    const p = makeProcessor();
    const N = Math.round(seconds * SR);
    const input = new Float32Array(N);
    for (let n = 0; n < N; n++) input[n] = mic(n / SR);
    const output = new Float32Array(N);
    for (let n = 0; n < N; n += 128) {
        const inp = input.subarray(n, n + 128);
        const out = output.subarray(n, n + 128);
        p.process([[inp]], [[out]], { frequency: [frequency] });
    }
    return { input, output, N };
}

const fmt = (v) => v.toFixed(4);

// 1. Bleed only: ison amp 0.1 at an arbitrary phase and level, as if it came
// through speakers and a room.
{
    const f = 220;
    const { input, output, N } = run({
        seconds: 3,
        frequency: f,
        mic: (t) => 0.1 * Math.sin(2 * Math.PI * f * t + 1.1),
    });
    console.log("1. bleed only (amp 0.1 @ 220 Hz)");
    for (const [label, from, to] of [
        ["0-0.25s", 0, SR * 0.25],
        ["0.5-1s", SR * 0.5, SR],
        ["2-3s", SR * 2, N],
    ]) {
        console.log(
            `   ${label}: in=${fmt(powerAt(input, f, from, to))} out=${fmt(powerAt(output, f, from, to))}`,
        );
    }
}

// 2. Bleed + voice a fifth above (330 Hz, amp 0.2).
{
    const f = 220;
    const v = 330;
    const { output, N } = run({
        seconds: 3,
        frequency: f,
        mic: (t) =>
            0.1 * Math.sin(2 * Math.PI * f * t + 0.4) +
            0.2 * Math.sin(2 * Math.PI * v * t),
    });
    console.log("2. bleed 220 Hz (0.1) + voice 330 Hz (0.2)");
    console.log(
        `   2-3s residual @220=${fmt(powerAt(output, f, SR * 2, N))}  voice @330=${fmt(powerAt(output, v, SR * 2, N))} (expect ~0 / ~0.2)`,
    );
}

// 3. Voice with vibrato centred on the ison (+/- 5 Hz at 5.5 Hz rate), bleed
// present.
{
    const f = 220;
    const { input, output, N } = run({
        seconds: 4,
        frequency: f,
        mic: (t) => {
            const phase =
                2 * Math.PI * f * t -
                (5 / 5.5) * Math.cos(2 * Math.PI * 5.5 * t);
            return 0.1 * Math.sin(2 * Math.PI * f * t + 0.4) + 0.2 * Math.sin(phase);
        },
    });
    const rms = (x, from, to) => {
        let s = 0;
        for (let n = from; n < to; n++) s += x[n] * x[n];
        return Math.sqrt(s / (to - from));
    };
    console.log("3. voice w/ vibrato centred on ison + bleed");
    console.log(
        `   2-4s RMS in=${fmt(rms(input, SR * 2, N))} out=${fmt(rms(output, SR * 2, N))} (voice alone would be ${fmt(0.2 / Math.SQRT2)})`,
    );
}

// 4. Voice 3 Hz sharp of ison (steady), bleed present.
{
    const f = 220;
    const v = 223;
    const { output, N } = run({
        seconds: 3,
        frequency: f,
        mic: (t) =>
            0.1 * Math.sin(2 * Math.PI * f * t + 0.4) +
            0.2 * Math.sin(2 * Math.PI * v * t),
    });
    console.log("4. bleed 220 Hz + voice 223 Hz (3 Hz sharp)");
    console.log(
        `   2-3s residual @220=${fmt(powerAt(output, f, SR * 2, N))}  voice @223=${fmt(powerAt(output, v, SR * 2, N))} (voice amp 0.2)`,
    );
}

// 5. Ison changes mid-stream: 220 -> 247.5 Hz.
{
    const p = makeProcessor();
    const N = SR * 4;
    const input = new Float32Array(N);
    const output = new Float32Array(N);
    const freqAt = (n) => (n < SR * 2 ? 220 : 247.5);
    let phase = 0;
    for (let n = 0; n < N; n++) {
        phase += (2 * Math.PI * freqAt(n)) / SR;
        input[n] = 0.1 * Math.sin(phase + 0.7);
    }
    for (let n = 0; n < N; n += 128) {
        p.process(
            [[input.subarray(n, n + 128)]],
            [[output.subarray(n, n + 128)]],
            { frequency: [freqAt(n)] },
        );
    }
    const rmsRange = (x, from, to) => {
        let s = 0;
        for (let n = from; n < to; n++) s += x[n] * x[n];
        return Math.sqrt(s / (to - from));
    };
    console.log("5. ison change 220 -> 247.5 Hz at t=2s (bleed amp 0.1, RMS 0.0707)");
    for (const [label, a, b] of [
        ["2.0-2.1s", 2, 2.1],
        ["2.1-2.5s", 2.1, 2.5],
        ["2.5-3s", 2.5, 3],
        ["3-4s", 3, 4],
    ]) {
        console.log(
            `   ${label}: out RMS=${fmt(rmsRange(output, SR * a, SR * b))}`,
        );
    }
}

// 6. Passthrough with frequency 0.
{
    const { input, output } = run({
        seconds: 0.1,
        frequency: 0,
        mic: (t) => 0.2 * Math.sin(2 * Math.PI * 300 * t),
    });
    let maxDiff = 0;
    for (let n = 0; n < input.length; n++)
        maxDiff = Math.max(maxDiff, Math.abs(input[n] - output[n]));
    console.log(`6. passthrough with no ison: max diff = ${maxDiff}`);
}
