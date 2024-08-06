import * as tinyapl from './tinyapl.js';
import * as wav from './wav.js';
function makeFunction(fn) {
    let listeners = [];
    const register = (l) => { listeners.push(l); };
    const done = () => { listeners = []; };
    const runListeners = async (...args) => { for (const l of listeners)
        await l(...args); };
    return { register, done, fn: (...args) => fn(runListeners, ...args) };
}
function toImageData(a, name) {
    if (a.shape.length !== 2 && a.shape.length !== 3)
        return { code: tinyapl.errors.rank, message: `${name} expects arrays of rank 2 or 3` };
    const fuzzy = a.shape.length === 2;
    const els = fuzzy ? 1 : a.shape.at(-1);
    if (![1, 2, 3, 4].includes(els))
        return { code: tinyapl.errors.length, message: `${name}: third axis must have length 1, 2, 3 or 4` };
    const data = new ImageData(a.shape[1], a.shape[0]);
    for (let y = 0; y < a.shape[0]; y++)
        for (let x = 0; x < a.shape[1]; x++) {
            const dIdx = (x * a.shape[0] + y) * 4;
            const uIdx = (x * a.shape[0] + y) * els;
            if (fuzzy) {
                data.data[dIdx + 0] = data.data[dIdx + 1] = data.data[dIdx + 2] = a.contents[uIdx + 0][0] * 255;
                data.data[dIdx + 3] = 255;
            }
            else if (els === 1) {
                data.data[dIdx + 0] = data.data[dIdx + 1] = data.data[dIdx + 2] = a.contents[uIdx + 0][0];
                data.data[dIdx + 3] = 255;
            }
            else if (els === 2) {
                data.data[dIdx + 0] = data.data[dIdx + 1] = data.data[dIdx + 2] = a.contents[uIdx + 0][0];
                data.data[dIdx + 3] = a.contents[uIdx + 1][0];
            }
            else if (els === 3) {
                data.data[dIdx + 0] = a.contents[uIdx + 0][0];
                data.data[dIdx + 1] = a.contents[uIdx + 1][0];
                data.data[dIdx + 2] = a.contents[uIdx + 2][0];
                data.data[dIdx + 3] = 255;
            }
            else {
                data.data[dIdx + 0] = a.contents[uIdx + 0][0];
                data.data[dIdx + 1] = a.contents[uIdx + 1][0];
                data.data[dIdx + 2] = a.contents[uIdx + 2][0];
                data.data[dIdx + 3] = a.contents[uIdx + 3][0];
            }
        }
    return data;
}
let imageId = 0;
export const { register: rCreateImage, done: dCreateImage, fn: qCreateImage } = makeFunction(async (runListeners, x, y) => {
    if (y)
        return { code: tinyapl.errors.domain, message: '⎕CreateImage must be called monadically' };
    if (x.shape.length !== 0 && x.shape.length !== 1)
        return { code: tinyapl.errors.rank, message: '⎕CreateImage expects arrays of rank 0 or 1' };
    if (x.shape.length === 1 && x.contents.length !== 2)
        return { code: tinyapl.errors.length, message: '⎕CreateImage expects a scalar or a two-element vector' };
    const height = x.contents[0][0];
    const width = x.contents[x.shape.length][0];
    const id = ++imageId;
    try {
        await runListeners(id, width, height);
    }
    catch (ex) {
        console.error(ex);
        return { code: tinyapl.errors.user, message: ex.message };
    }
    return { shape: [], contents: [[id, 0]] };
});
export const { register: rDisplayImage, done: dDisplayImage, fn: qDisplayImage } = makeFunction(async (runListeners, x, y) => {
    let id, a;
    if (y) {
        a = y;
        if (x.shape.length !== 0)
            return { code: tinyapl.errors.rank, message: '⎕DispayImage left argument must be a scalar natural' };
        id = x.contents[0][0];
    }
    else
        a = x;
    const data = toImageData(a, '⎕DisplayImage');
    if ('code' in data)
        return data;
    try {
        await runListeners(id, data);
    }
    catch (ex) {
        console.error(ex);
        return { code: tinyapl.errors.user, message: ex.message };
    }
    return { shape: [0], contents: [] };
});
export const { register: rScatterPlot, done: dScatterPlot, fn: qScatterPlot } = makeFunction(async (runListeners, x, y) => {
    let mode = 'markers', arr;
    if (y) {
        mode = await tinyapl.joinString(x.contents);
        arr = y;
    }
    else
        arr = x;
    if (arr.shape.length !== 2 && arr.shape.length !== 3)
        return { code: tinyapl.errors.rank, message: '⎕ScatterPlot expects arrays of rank 2 or 3' };
    if (arr.shape.at(-1) !== 2)
        return { code: tinyapl.errors.length, message: '⎕ScatterPlot argument last axis must be of length 2' };
    const xs = [], ys = [];
    if (arr.shape.length === 2) {
        const x1 = [], y1 = [];
        for (let i = 0; i < 2 * arr.shape[0]; i += 2) {
            x1.push(arr.contents[i][0]);
            y1.push(arr.contents[i + 1][0]);
        }
        xs.push(x1);
        ys.push(y1);
    }
    else {
        for (let j = 0; j < 2 * arr.shape[1] * arr.shape[0]; j += 2 * arr.shape[1]) {
            const x1 = [], y1 = [];
            for (let i = 0; i < 2 * arr.shape[1]; i += 2) {
                x1.push(arr.contents[j + i][0]);
                y1.push(arr.contents[j + i + 1][0]);
            }
            xs.push(x1);
            ys.push(y1);
        }
    }
    try {
        await runListeners(xs, ys, mode);
    }
    catch (ex) {
        console.error(ex);
        return { code: tinyapl.errors.user, message: ex.message };
    }
    return { shape: [0], contents: [] };
});
export const { register: rPlayAudio, done: dPlayAudio, fn: qPlayAudio } = makeFunction(async (runListeners, x, y) => {
    let sampleRate, arr;
    if (y) {
        if (x.shape.length !== 0)
            return { code: tinyapl.errors.rank, message: '⎕PlayAudio left argument must be scalar' };
        sampleRate = Math.floor(x.contents[0][0]);
        arr = y;
    }
    else {
        arr = x;
        sampleRate = 44100;
    }
    if (arr.shape.length !== 1 && arr.shape.length !== 2)
        return { code: tinyapl.errors.rank, message: '⎕PlayAudio expects arrays of rank 1 or 2' };
    const channels = arr.shape.length === 1 ? 1 : arr.shape[0];
    const length = arr.shape.at(-1);
    const bufs = new Array(channels).fill(0).map(_ => new Float32Array(length));
    for (let ch = 0; ch < channels; ch++)
        for (let b = 0; b < length; b++)
            bufs[ch][b] = arr.contents[ch * length + b][0];
    try {
        await runListeners(wav.encode(sampleRate, bufs));
    }
    catch (ex) {
        console.error(ex);
        return { code: tinyapl.errors.user, message: ex.message };
    }
    return { shape: [0], contents: [] };
});
export async function qFetch(x, y) {
    let u, m;
    if (y) {
        u = y;
        m = x;
    }
    else {
        u = x;
        m = undefined;
    }
    if (u.shape.length > 1)
        return { code: tinyapl.errors.rank, message: '⎕Fetch expects character vectors' };
    const url = await tinyapl.joinString(u.contents);
    let response;
    try {
        response = await fetch(url);
    }
    catch (ex) {
        console.error(ex);
        return { code: tinyapl.errors.user, message: ex.message };
    }
    if (m) {
        if (m.shape.length !== 0)
            return { code: tinyapl.errors.domain, message: '⎕Fetch left argument must be one of ⟨1⋄¯1⋄0ᴊ1⋄0ᴊ¯1⟩⊞⟨8⋄16⋄32⟩ or 1' };
        const mode = m.contents[0].join(';');
        const buf = await response.arrayBuffer();
        const view = new DataView(buf);
        const result = [];
        switch (mode) {
            case '1:0':
                for (let i = 0; i < view.byteLength; i += 1) {
                    const u = view.getUint8(i);
                    for (let bi = 0; bi < 8; bi++) {
                        result.push(Number((u & (1 << bi)) !== 0));
                    }
                }
                break;
            case '8;0':
            case '0;8':
                for (let i = 0; i < view.byteLength; i += 1)
                    result.push(view.getUint8(i));
                break;
            case '-8;0':
            case '0;-8':
                for (let i = 0; i < view.byteLength; i += 1)
                    result.push(view.getInt8(i));
                break;
            case '16;0':
                for (let i = 0; i < view.byteLength; i += 2)
                    result.push(view.getUint16(i, true));
                break;
            case '-16:0':
                for (let i = 0; i < view.byteLength; i += 2)
                    result.push(view.getInt16(i, true));
                break;
            case '0;16':
                for (let i = 0; i < view.byteLength; i += 2)
                    result.push(view.getUint16(i, false));
                break;
            case '0:-16':
                for (let i = 0; i < view.byteLength; i += 2)
                    result.push(view.getInt16(i, false));
                break;
            case '32;0':
                for (let i = 0; i < view.byteLength; i += 2)
                    result.push(view.getUint32(i, true));
                break;
            case '-32:0':
                for (let i = 0; i < view.byteLength; i += 2)
                    result.push(view.getInt32(i, true));
                break;
            case '0;32':
                for (let i = 0; i < view.byteLength; i += 2)
                    result.push(view.getUint32(i, false));
                break;
            case '0:-32':
                for (let i = 0; i < view.byteLength; i += 2)
                    result.push(view.getInt32(i, false));
                break;
        }
        return { shape: [result.length], contents: result.map(r => [r, 0]) };
    }
    else {
        const text = await response.text();
        return { shape: [text.length], contents: await tinyapl.splitString(text) };
    }
}
