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
function handleEx(ex) {
    if (!(ex instanceof Error) && typeof ex === 'object' && ex !== null && 'code' in ex && 'message' in ex)
        return ex;
    else if (ex instanceof Error)
        return { code: tinyapl.errors.user, message: ex.message };
    else
        return { code: tinyapl.errors.user, message: ex.toString() };
}
function monad(fn, repr) {
    return {
        type: 'function',
        repr,
        monad: async (y) => {
            try {
                return await fn(y);
            }
            catch (ex) {
                return handleEx(ex);
            }
        },
    };
}
function dyad(fn, repr) {
    return {
        type: 'function',
        repr,
        dyad: async (x, y) => {
            try {
                return await fn(x, y);
            }
            catch (ex) {
                return handleEx(ex);
            }
        },
    };
}
function ambivalent(m, d, repr) {
    return {
        type: 'function',
        repr,
        monad: async (x) => {
            try {
                return await m(x);
            }
            catch (ex) {
                return handleEx(ex);
            }
        },
        dyad: async (x, y) => {
            try {
                return await d(x, y);
            }
            catch (ex) {
                return handleEx(ex);
            }
        },
    };
}
function ambivalent1(fn, repr) {
    return ambivalent(fn, fn, repr);
}
function makeMonad(fn, repr) {
    const { register, done, fn: fn1 } = makeFunction(fn);
    return { register, done, fn: monad(fn1, repr) };
}
function makeDyad(fn, repr) {
    const { register, done, fn: fn1 } = makeFunction(fn);
    return { register, done, fn: dyad(fn1, repr) };
}
function makeAmbivalent(m, d, repr) {
    let listeners = [];
    const register = (l) => { listeners.push(l); };
    const done = () => { listeners = []; };
    const runListeners = async (...args) => { for (const l of listeners)
        await l(...args); };
    return { register, done, fn: ambivalent(y => m(runListeners, y), (x, y) => d(runListeners, x, y), repr) };
}
function makeAmbivalent1(fn, repr) {
    return makeAmbivalent(fn, fn, repr);
}
function toImageData(a, name) {
    if (a.shape.length !== 2 && a.shape.length !== 3)
        throw { code: tinyapl.errors.rank, message: `${name} expects arrays of rank 2 or 3` };
    const els = a.shape.length === 2 ? 1 : a.shape.at(-1);
    if (![1, 2, 3, 4].includes(els))
        throw { code: tinyapl.errors.length, message: `${name}: third axis must have length 1, 2, 3 or 4` };
    const data = new ImageData(a.shape[1], a.shape[0]);
    for (let y = 0; y < a.shape[0]; y++)
        for (let x = 0; x < a.shape[1]; x++) {
            const dIdx = (x * a.shape[0] + y) * 4;
            const uIdx = (x * a.shape[0] + y) * els;
            if (els === 1) {
                data.data[dIdx + 0] = data.data[dIdx + 1] = data.data[dIdx + 2] = a.contents[uIdx + 0][0] * 255;
                data.data[dIdx + 3] = 255;
            }
            else if (els === 2) {
                data.data[dIdx + 0] = data.data[dIdx + 1] = data.data[dIdx + 2] = a.contents[uIdx + 0][0] * 255;
                data.data[dIdx + 3] = a.contents[uIdx + 1][0] * 255;
            }
            else if (els === 3) {
                data.data[dIdx + 0] = a.contents[uIdx + 0][0] * 255;
                data.data[dIdx + 1] = a.contents[uIdx + 1][0] * 255;
                data.data[dIdx + 2] = a.contents[uIdx + 2][0] * 255;
                data.data[dIdx + 3] = 255;
            }
            else {
                data.data[dIdx + 0] = a.contents[uIdx + 0][0] * 255;
                data.data[dIdx + 1] = a.contents[uIdx + 1][0] * 255;
                data.data[dIdx + 2] = a.contents[uIdx + 2][0] * 255;
                data.data[dIdx + 3] = a.contents[uIdx + 3][0] * 255;
            }
        }
    return data;
}
let imageId = 0;
export const { register: rCreateImage, done: dCreateImage, fn: qCreateImage } = makeDyad(async (runListeners, x, y) => {
    if (y.shape.length !== 0 && x.shape.length !== 1)
        throw { code: tinyapl.errors.rank, message: '⎕CreateImage expects arrays of rank 0 or 1' };
    if (y.shape.length === 1 && x.contents.length !== 2)
        throw { code: tinyapl.errors.length, message: '⎕CreateImage expects a scalar or a two-element vector' };
    const height = y.contents[0][0];
    const width = y.contents[x.shape.length][0];
    const id = ++imageId;
    await runListeners(id, width, height);
    return { type: 'array', shape: [], contents: [[id, 0]] };
}, '⎕CreateImage');
export const { register: rDisplayImage, done: dDisplayImage, fn: qDisplayImage } = makeAmbivalent1(async (runListeners, x, y) => {
    let id, a;
    if (y) {
        a = y;
        if (x.shape.length !== 0)
            throw { code: tinyapl.errors.rank, message: '⎕DispayImage left argument must be a scalar natural' };
        id = x.contents[0][0];
    }
    else
        a = x;
    const data = toImageData(a, '⎕DisplayImage');
    await runListeners(id, data);
    throw { type: 'array', shape: [0], contents: [] };
}, '⎕DisplayImage');
export const { register: rPlayAnimation, done: dPlayAnimation, fn: qPlayAnimation } = makeAmbivalent1(async (runListeners, x, y) => {
    let delay, arr;
    if (y) {
        arr = y;
        if (x.shape.length !== 0)
            throw { code: tinyapl.errors.rank, message: '⎕PlayAnimation left argument must be a scalar' };
        delay = x.contents[0][0];
    }
    else {
        arr = x;
        delay = 0.1;
    }
    if (arr.shape.length !== 3 && arr.shape.length !== 4)
        throw { code: tinyapl.errors.rank, message: '⎕PlayAnimation expects arrays of rank 3 or 4' };
    const [frames] = arr.shape;
    const len = arr.shape.slice(1).reduce((a, b) => a * b, 1);
    const datas = [];
    for (let idx = 0; idx < frames; idx++)
        datas.push(toImageData({ type: 'array', shape: arr.shape.slice(1), contents: arr.contents.slice(idx * len).slice(0, len) }, '⎕PlayAnimation'));
    await runListeners(delay, datas);
    return { type: 'array', shape: [0], contents: [] };
}, '⎕PlayAnimation');
export const { register: rScatterPlot, done: dScatterPlot, fn: qScatterPlot } = makeAmbivalent1(async (runListeners, x, y) => {
    let mode = 'markers', arr;
    if (y) {
        mode = await tinyapl.joinString(x.contents);
        arr = y;
    }
    else
        arr = x;
    if (arr.shape.length !== 2 && arr.shape.length !== 3)
        throw { code: tinyapl.errors.rank, message: '⎕ScatterPlot expects arrays of rank 2 or 3' };
    if (arr.shape.at(-1) !== 2)
        throw { code: tinyapl.errors.length, message: '⎕ScatterPlot argument last axis must be of length 2' };
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
    await runListeners(xs, ys, mode);
    return { type: 'array', shape: [0], contents: [] };
}, '⎕ScatterPlot');
export const { register: rPlayAudio, done: dPlayAudio, fn: qPlayAudio } = makeAmbivalent1(async (runListeners, x, y) => {
    let sampleRate, arr;
    if (y) {
        if (x.shape.length !== 0)
            throw { code: tinyapl.errors.rank, message: '⎕PlayAudio left argument must be scalar' };
        sampleRate = Math.floor(x.contents[0][0]);
        arr = y;
    }
    else {
        arr = x;
        sampleRate = 44100;
    }
    if (arr.shape.length !== 1 && arr.shape.length !== 2)
        throw { code: tinyapl.errors.rank, message: '⎕PlayAudio expects arrays of rank 1 or 2' };
    const channels = arr.shape.length === 1 ? 1 : arr.shape[0];
    const length = arr.shape.at(-1);
    const bufs = new Array(channels).fill(0).map(_ => new Float32Array(length));
    for (let ch = 0; ch < channels; ch++)
        for (let b = 0; b < length; b++)
            bufs[ch][b] = arr.contents[ch * length + b][0];
    await runListeners(wav.encode(sampleRate, bufs));
    return { type: 'array', shape: [0], contents: [] };
}, '⎕PlayAudio');
export const qFetch = ambivalent(async (u) => {
    if (u.shape.length > 1)
        throw { code: tinyapl.errors.rank, message: '⎕Fetch expects character vectors' };
    const url = await tinyapl.joinString(u.contents);
    const text = await fetch(url).then(res => res.text());
    return { type: 'array', shape: [text.length], contents: await tinyapl.splitString(text) };
}, async (m, u) => {
    if (u.shape.length > 1)
        throw { code: tinyapl.errors.rank, message: '⎕Fetch expects character vectors' };
    const url = await tinyapl.joinString(u.contents);
    if (m.shape.length !== 0)
        throw { code: tinyapl.errors.domain, message: '⎕Fetch left argument must be one of ⟨1⋄¯1⋄0ᴊ1⋄0ᴊ¯1⟩⊞⟨8⋄16⋄32⟩, ⟨1⋄0ᴊ1⟩⊞⟨0.32⋄0.64⟩ or 1' };
    const mode = m.contents[0].join(';');
    const buf = await fetch(url).then(res => res.arrayBuffer());
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
        case '0.32:0':
            for (let i = 0; i < view.byteLength; i += 4)
                result.push(view.getFloat32(i, true));
            break;
        case '0:0.32':
            for (let i = 0; i < view.byteLength; i += 4)
                result.push(view.getFloat32(i, false));
            break;
        case '0.64:0':
            for (let i = 0; i < view.byteLength; i += 8)
                result.push(view.getFloat64(i, true));
            break;
        case '0:0.64':
            for (let i = 0; i < view.byteLength; i += 8)
                result.push(view.getFloat64(i, false));
            break;
    }
    return { type: 'array', shape: [result.length], contents: result.map(r => [r, 0]) };
}, '⎕Fetch');
