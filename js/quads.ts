import * as tinyapl from './tinyapl.js';
import * as wav from './wav.js';

async function callErr<Ret, Args extends unknown[]>(fn: (...args: Args) => PromiseLike<Ret | tinyapl.Err>, ...args: Args): Promise<Ret> {
	const res = await fn(...args);
	if (typeof res === 'object' && res !== null && 'code' in res && 'message' in res) throw res;
	return res;
}

function makeFunction<Args extends unknown[], Ret, ListenerArgs extends unknown[]>(fn: (runListeners: (...args: ListenerArgs) => Promise<void>, ...args: Args) => Ret) {
	let listeners: ((...args: ListenerArgs) => Promise<void>)[] = [];
	const register = (l: (...args: ListenerArgs) => Promise<void>) => { listeners.push(l); };
	const done = () => { listeners = []; };
	const runListeners = async (...args: ListenerArgs) => { for (const l of listeners) await l(...args); }
	return { register, done, fn: (...args: Args) => fn(runListeners, ...args) };
}

function handleEx(ex: unknown): tinyapl.Err {
	if (!(ex instanceof Error) && typeof ex === 'object' && ex !== null && 'code' in ex && 'message' in ex) return ex as tinyapl.Err;
	else if (ex instanceof Error) return { code: tinyapl.errors.user, message: ex.message };
	else return { code: tinyapl.errors.user, message: (ex as any).toString() };
}

function monad(fn: (y: tinyapl.Arr) => PromiseLike<tinyapl.Arr>, repr: string): tinyapl.Fun {
	return {
		type: 'function',
		repr,
		monad: async y => {
			try {
				return await fn(y);
			} catch (ex) {
				return handleEx(ex);
			}
		},
	};
}

function dyad(fn: (x: tinyapl.Arr, y: tinyapl.Arr) => PromiseLike<tinyapl.Arr>, repr: string): tinyapl.Fun {
	return {
		type: 'function',
		repr,
		dyad: async (x, y) => {
			try {
				return await fn(x, y);
			} catch (ex) {
				return handleEx(ex);
			}
		},
	};
}

function ambivalent(m: (y: tinyapl.Arr) => PromiseLike<tinyapl.Arr>, d: (x: tinyapl.Arr, y: tinyapl.Arr) => PromiseLike<tinyapl.Arr>, repr: string): tinyapl.Fun {
	return {
		type: 'function',
		repr,
		monad: async x => {
			try {
				return await m(x)
			} catch (ex) {
				return handleEx(ex);
			}
		},
		dyad: async (x, y) => {
			try {
				return await d(x, y);
			} catch (ex) {
				return handleEx(ex);
			}
		},
	};
}

function ambivalent1(fn: (x: tinyapl.Arr, y?: tinyapl.Arr) => PromiseLike<tinyapl.Arr>, repr: string) {
	return ambivalent(fn, fn, repr);
}

function adverbArr(adv: (n: tinyapl.Arr) => PromiseLike<tinyapl.Fun>, repr: string): tinyapl.Adv {
	return {
		type: 'adverb',
		repr,
		array: async n => {
			try {
				return await adv(n);
			} catch (ex) {
				return handleEx(ex);
			}
		},
	}
}

function adverbFun(adv: (f: tinyapl.Fun) => PromiseLike<tinyapl.Fun>, repr: string): tinyapl.Adv {
	return {
		type: 'adverb',
		repr,
		function: async f => {
			try {
				return await adv(f);
			} catch (ex) {
				return handleEx(ex);
			}
		},
	}
}

function adverb(arr: (n: tinyapl.Arr) => PromiseLike<tinyapl.Fun>, fn: (f: tinyapl.Fun) => PromiseLike<tinyapl.Fun>, repr: string): tinyapl.Adv {
	return {
		type: 'adverb',
		repr,
		array: async n => {
			try {
				return await arr(n);
			} catch (ex) {
				return handleEx(ex);
			}
		},
		function: async f => {
			try {
				return await fn!(f);
			} catch (ex) {
				return handleEx(ex);
			}
		},
	}
}

function adverb1(adv: (u: tinyapl.Arr | tinyapl.Fun) => PromiseLike<tinyapl.Fun>, repr: string) {
	return adverb(adv, adv, repr);
}

function makeMonad<ListenerArgs extends unknown[]>(fn: (listener: (...args: ListenerArgs) => PromiseLike<void>, y: tinyapl.Arr) => PromiseLike<tinyapl.Arr>, repr: string) {
	const { register, done, fn: fn1 } = makeFunction<[tinyapl.Arr], PromiseLike<tinyapl.Arr>, ListenerArgs>(fn);
	return { register, done, fn: monad(fn1, repr) };
}

function makeDyad<ListenerArgs extends unknown[]>(fn: (listener: (...args: ListenerArgs) => PromiseLike<void>, x: tinyapl.Arr, y: tinyapl.Arr) => PromiseLike<tinyapl.Arr>, repr: string) {
	const { register, done, fn: fn1 } = makeFunction<[tinyapl.Arr, tinyapl.Arr], PromiseLike<tinyapl.Arr>, ListenerArgs>(fn);
	return { register, done, fn: dyad(fn1, repr) };
}

function makeAmbivalent<ListenerArgs extends unknown[]>(m: (listener: (...args: ListenerArgs) => PromiseLike<void>, y: tinyapl.Arr) => PromiseLike<tinyapl.Arr>, d: (listener: (...args: ListenerArgs) => PromiseLike<void>, x: tinyapl.Arr, y: tinyapl.Arr) => PromiseLike<tinyapl.Arr>, repr: string) {
	let listeners: ((...args: ListenerArgs) => Promise<void>)[] = [];
	const register = (l: (...args: ListenerArgs) => Promise<void>) => { listeners.push(l); };
	const done = () => { listeners = []; };
	const runListeners = async (...args: ListenerArgs) => { for (const l of listeners) await l(...args); }
	return { register, done, fn: ambivalent(y => m(runListeners, y), (x, y) => d(runListeners, x, y), repr) };
}

function makeAmbivalent1<ListenerArgs extends unknown[]>(fn: (listener: (...args: ListenerArgs) => PromiseLike<void>, x: tinyapl.Arr, y?: tinyapl.Arr) => PromiseLike<tinyapl.Arr>, repr: string) {
	return makeAmbivalent(fn, fn, repr);
}

function makeAdverb<ListenerArgs extends unknown[]>(arr: (listener: (...args: ListenerArgs) => PromiseLike<void>, n: tinyapl.Arr) => PromiseLike<tinyapl.Fun>, fn: (listener: (...args: ListenerArgs) => PromiseLike<void>, f: tinyapl.Fun) => PromiseLike<tinyapl.Fun>, repr: string) {
	let listeners: ((...args: ListenerArgs) => Promise<void>)[] = [];
	const register = (l: (...args: ListenerArgs) => Promise<void>) => { listeners.push(l); };
	const done = () => { listeners = []; };
	const runListeners = async (...args: ListenerArgs) => { for (const l of listeners) await l(...args); }
	return { register, done, fn: adverb(n => arr(runListeners, n), f => fn(runListeners, f), repr) };
}

function makeAdverb1<ListenerArgs extends unknown[]>(arr: (listener: (...args: ListenerArgs) => PromiseLike<void>, u: tinyapl.Arr | tinyapl.Fun) => PromiseLike<tinyapl.Fun>, repr: string) {
	return makeAdverb(arr, arr, repr);
}

function toImageData(a: tinyapl.Arr, name: string): ImageData {
	if (a.shape.length !== 2 && a.shape.length !== 3) throw { code: tinyapl.errors.rank, message: `${name} expects arrays of rank 2 or 3` };
	const els = a.shape.length === 2 ? 1 : a.shape.at(-1)!;
	if (![1, 2, 3, 4].includes(els)) throw { code: tinyapl.errors.length, message: `${name}: third axis must have length 1, 2, 3 or 4` };
	const data = new ImageData(a.shape[1], a.shape[0]);
	for (let y = 0; y < a.shape[0]; y++)
		for (let x = 0; x < a.shape[1]; x++) {
			const dIdx = (x * a.shape[0] + y) * 4;
			const uIdx = (x * a.shape[0] + y) * els;
			if (els === 1) {
				data.data[dIdx + 0] = data.data[dIdx + 1] = data.data[dIdx + 2] = (a.contents[uIdx + 0] as tinyapl.Complex)[0] * 255;
				data.data[dIdx + 3] = 255;
			} else if (els === 2) {
				data.data[dIdx + 0] = data.data[dIdx + 1] = data.data[dIdx + 2] = (a.contents[uIdx + 0] as tinyapl.Complex)[0] * 255;
				data.data[dIdx + 3] = (a.contents[uIdx + 1] as tinyapl.Complex)[0] * 255;
			} else if (els === 3) {
				data.data[dIdx + 0] = (a.contents[uIdx + 0] as tinyapl.Complex)[0] * 255;
				data.data[dIdx + 1] = (a.contents[uIdx + 1] as tinyapl.Complex)[0] * 255;
				data.data[dIdx + 2] = (a.contents[uIdx + 2] as tinyapl.Complex)[0] * 255;
				data.data[dIdx + 3] = 255;
			} else {
				data.data[dIdx + 0] = (a.contents[uIdx + 0] as tinyapl.Complex)[0] * 255;
				data.data[dIdx + 1] = (a.contents[uIdx + 1] as tinyapl.Complex)[0] * 255;
				data.data[dIdx + 2] = (a.contents[uIdx + 2] as tinyapl.Complex)[0] * 255;
				data.data[dIdx + 3] = (a.contents[uIdx + 3] as tinyapl.Complex)[0] * 255;
			}
		}
	return data;
}

let imageId = 0;

export const { register: rCreateImage, done: dCreateImage, fn: qCreateImage } = makeDyad<[number, number, number]>(async (runListeners, x: tinyapl.Arr, y: tinyapl.Arr) => {
	if (y.shape.length !== 0 && x.shape.length !== 1) throw { code: tinyapl.errors.rank, message: '⎕CreateImage expects arrays of rank 0 or 1' };
	if (y.shape.length === 1 && x.contents.length !== 2) throw { code: tinyapl.errors.length, message: '⎕CreateImage expects a scalar or a two-element vector' };
	const height = (y.contents[0] as tinyapl.Complex)[0]
	const width = (y.contents[x.shape.length] as tinyapl.Complex)[0];
	const id = ++imageId;
	await runListeners(id, width, height);
	return { type: 'array', shape: [], contents: [[id, 0]] };
}, '⎕CreateImage');

export const { register: rDisplayImage, done: dDisplayImage, fn: qDisplayImage } = makeAmbivalent1<[number | undefined, ImageData]>(async (runListeners, x: tinyapl.Arr, y?: tinyapl.Arr) => {
	let id, a;
	if (y) {
		a = y;
		if (x.shape.length !== 0) throw { code: tinyapl.errors.rank, message: '⎕DispayImage left argument must be a scalar natural' };
		id = (x.contents[0] as tinyapl.Complex)[0];
	} else a = x;
	const data = toImageData(a, '⎕DisplayImage');
	await runListeners(id, data);
	return { type: 'array', shape: [0], contents: [] };
}, '⎕DisplayImage');

export const { register: rPlayAnimation, done: dPlayAnimation, fn: qPlayAnimation } = makeAmbivalent1<[number, ImageData[]]>(async (runListeners, x: tinyapl.Arr, y?: tinyapl.Arr) => {
	let delay, arr;
	if (y) {
		arr = y;
		if (x.shape.length !== 0) throw { code: tinyapl.errors.rank, message: '⎕PlayAnimation left argument must be a scalar' };
		delay = (x.contents[0] as tinyapl.Complex)[0];
	} else {
		arr = x;
		delay = 0.1;
	}
	if (arr.shape.length !== 3 && arr.shape.length !== 4) throw { code: tinyapl.errors.rank, message: '⎕PlayAnimation expects arrays of rank 3 or 4' };
	const [frames] = arr.shape;
	const len = arr.shape.slice(1).reduce((a, b) => a * b, 1);
	const datas: ImageData[] = [];
	for (let idx = 0; idx < frames; idx++)
		datas.push(toImageData({ type: 'array', shape: arr.shape.slice(1), contents: arr.contents.slice(idx * len).slice(0, len) }, '⎕PlayAnimation'));
	await runListeners(delay, datas);
	return { type: 'array', shape: [0], contents: [] };
}, '⎕PlayAnimation');

export const { register: rScatterPlot, done: dScatterPlot, fn: qScatterPlot } = makeAmbivalent1<[number[][], number[][], string]>(async (runListeners, x: tinyapl.Arr, y?: tinyapl.Arr) => {
	let mode = 'markers', arr;
	if (y) {
		mode = await tinyapl.joinString(x.contents as string[]);
		arr = y;
	} else arr = x;
	if (arr.shape.length !== 2 && arr.shape.length !== 3) throw { code: tinyapl.errors.rank, message: '⎕ScatterPlot expects arrays of rank 2 or 3' };
	if (arr.shape.at(-1) !== 2) throw { code: tinyapl.errors.length, message: '⎕ScatterPlot argument last axis must be of length 2' };
	const xs = [], ys = [];
	if (arr.shape.length === 2) {
		const x1 = [], y1 = [];
		for (let i = 0; i < 2 * arr.shape[0]; i += 2) {
			x1.push((arr.contents[i] as tinyapl.Complex)[0]);
			y1.push((arr.contents[i + 1] as tinyapl.Complex)[0]);
		}
		xs.push(x1);
		ys.push(y1);
	} else {
		for (let j = 0; j < 2 * arr.shape[1] * arr.shape[0]; j += 2 * arr.shape[1]) {
			const x1 = [], y1 = [];
			for (let i = 0; i < 2 * arr.shape[1]; i += 2) {
				x1.push((arr.contents[j + i] as tinyapl.Complex)[0]);
				y1.push((arr.contents[j + i + 1] as tinyapl.Complex)[0]);
			}
			xs.push(x1);
			ys.push(y1);
		}
	}
	await runListeners(xs, ys, mode);
	return { type: 'array', shape: [0], contents: [] };
}, '⎕ScatterPlot');

export const { register: rGraph, done: dGraph, fn: qGraph } = makeAdverb1<[[number, number][][], string[]]>(async (runListeners, u) => {
	const graphCounts = 500;
	let fns: tinyapl.Fun[] = [], labels: string[] = [];
	if (u.type === 'function') {
		fns = [u];
		labels = [u.repr];
	} else {
		if (u.shape.length === 0) {
			fns = [u.contents[0] as tinyapl.Fun];
			labels = [fns[0].repr];
		} else if (u.shape.length === 1) {
			for (const c of u.contents) {
				if (typeof c === 'object' && !Array.isArray(c) && c.type === 'function') {
					fns.push(c);
					labels.push(c.repr);
				} else if (typeof c === 'object' && !Array.isArray(c) && c.type === 'array' && c.shape.length === 1 && c.shape[0] === 2) {
					fns.push(c.contents[0] as tinyapl.Fun);
					labels.push(typeof c.contents[1] === 'string' ? c.contents[1] : await tinyapl.joinString((c.contents[1] as tinyapl.Arr).contents as string[]));
				} else throw { code: tinyapl.errors.domain, message: '⎕Graph left operand must be a function or a list of functions or pairs of functions and labels' };
			}
		}
	}
	const graph = async (start: number, end: number) => {
		let results: [number, number][][] = [];
		for (const f of fns) {
			results.push([]);			
			for (let n = start; n < end; n += (end - start) / graphCounts) {
				results.at(-1)!.push([n, ((await callErr(f.monad!, { type: 'array', shape: [], contents: [[n, 0]] })).contents[0] as tinyapl.Complex)[0]]);
			}
		}
		console.log(results);
		await runListeners(results, labels);
	};
	return ambivalent(async y => {
		if (y.shape.length !== 0) throw { code: tinyapl.errors.rank, message: '⎕Graph arguments must be scalar reals' };
		const end = (y.contents[0] as tinyapl.Complex)[0];
		await graph(0, end);
		return { type: 'array', shape: [0], contents: [] };
	}, async (x, y) => {
		if (x.shape.length !== 0) throw { code: tinyapl.errors.rank, message: '⎕Graph arguments must be scalar reals' };
		if (y.shape.length !== 0) throw { code: tinyapl.errors.rank, message: '⎕Graph arguments must be scalar reals' };
		const start = (x.contents[0] as tinyapl.Complex)[0];
		const end = (y.contents[0] as tinyapl.Complex)[0];
		await graph(start, end);
		return { type: 'array', shape: [0], contents: [] };
	}, '(' + await tinyapl.show(u) + ')' + '⎕_Graph');
}, '⎕_Graph');

export const { register: rPlayAudio, done: dPlayAudio, fn: qPlayAudio } = makeAmbivalent1<[ArrayBuffer]>(async (runListeners, x: tinyapl.Arr, y?: tinyapl.Arr) => {
	let sampleRate, arr;
	if (y) {
		if (x.shape.length !== 0) throw { code: tinyapl.errors.rank, message: '⎕PlayAudio left argument must be scalar' };
		sampleRate = Math.floor((x.contents[0] as tinyapl.Complex)[0]);
		arr = y;
	} else {
		arr = x;
		sampleRate = 44100;
	}
	if (arr.shape.length !== 1 && arr.shape.length !== 2) throw { code: tinyapl.errors.rank, message: '⎕PlayAudio expects arrays of rank 1 or 2' };
	const channels = arr.shape.length === 1 ? 1 : arr.shape[0];
	const length = arr.shape.at(-1)!;
	const bufs = new Array(channels).fill(0).map(_ => new Float32Array(length));
	for (let ch = 0; ch < channels; ch++)
		for (let b = 0; b < length; b++)
			bufs[ch][b] = (arr.contents[ch * length + b] as tinyapl.Complex)[0];
	await runListeners(wav.encode(sampleRate, bufs));
	return { type: 'array', shape: [0], contents: [] };
}, '⎕PlayAudio');

export const qFetch = ambivalent(async (u: tinyapl.Arr) => {
	if (u.shape.length > 1) throw { code: tinyapl.errors.rank, message: '⎕Fetch expects character vectors' };
	const url = await tinyapl.joinString(u.contents as string[]);
	const text = await fetch(url).then(res => res.text());
	return { type: 'array', shape: [text.length], contents: await tinyapl.splitString(text) };
}, async (m: tinyapl.Arr, u: tinyapl.Arr) => {
	if (u.shape.length > 1) throw { code: tinyapl.errors.rank, message: '⎕Fetch expects character vectors' };
	const url = await tinyapl.joinString(u.contents as string[]);
	if (m.shape.length !== 0) throw { code: tinyapl.errors.domain, message: '⎕Fetch left argument must be one of ⟨1⋄¯1⋄0ᴊ1⋄0ᴊ¯1⟩⊞⟨8⋄16⋄32⟩, ⟨1⋄0ᴊ1⟩⊞⟨0.32⋄0.64⟩ or 1' };
	const mode = (m.contents[0] as tinyapl.Complex).join(';');
	const buf = await fetch(url).then(res => res.arrayBuffer());
	const view = new DataView(buf);
	const result = [];
	switch (mode) {
		case '1;0':
			for (let i = 0; i < view.byteLength; i += 1) {
				const u = view.getUint8(i);
				for (let bi = 0; bi < 8; bi++) {
					result.push(Number((u & (1 << bi)) !== 0));
				}
			}
			break;
		case '8;0':
		case '0;8':
			for (let i = 0; i < view.byteLength; i += 1) result.push(view.getUint8(i));
			break;
		case '-8;0':
		case '0;-8':
			for (let i = 0; i < view.byteLength; i += 1) result.push(view.getInt8(i));
			break;
		case '16;0':
			for (let i = 0; i < view.byteLength; i += 2) result.push(view.getUint16(i, true));
			break;
		case '-16;0':
			for (let i = 0; i < view.byteLength; i += 2) result.push(view.getInt16(i, true));
			break;
		case '0;16':
			for (let i = 0; i < view.byteLength; i += 2) result.push(view.getUint16(i, false));
			break;
		case '0;-16':
			for (let i = 0; i < view.byteLength; i += 2) result.push(view.getInt16(i, false));
			break;
		case '32;0':
			for (let i = 0; i < view.byteLength; i += 2) result.push(view.getUint32(i, true));
			break;
		case '-32;0':
			for (let i = 0; i < view.byteLength; i += 2) result.push(view.getInt32(i, true));
			break;
		case '0;32':
			for (let i = 0; i < view.byteLength; i += 2) result.push(view.getUint32(i, false));
			break;
		case '0;-32':
			for (let i = 0; i < view.byteLength; i += 2) result.push(view.getInt32(i, false));
			break;
		case '0.32;0':
			for (let i = 0; i < view.byteLength; i += 4) result.push(view.getFloat32(i, true));
			break;
		case '0;0.32':
			for (let i = 0; i < view.byteLength; i += 4) result.push(view.getFloat32(i, false));
			break;
		case '0.64;0':
			for (let i = 0; i < view.byteLength; i += 8) result.push(view.getFloat64(i, true));
			break;
		case '0;0.64':
			for (let i = 0; i < view.byteLength; i += 8) result.push(view.getFloat64(i, false));
			break;
	}
	return { type: 'array', shape: [result.length], contents: result.map(r => [r, 0]) };
}, '⎕Fetch');
