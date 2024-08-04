import * as tinyapl from './tinyapl.js';

function makeFunction(fn) {
	let listeners = [];
	const register = l => { listeners.push(l); };
	const done = () => { listeners = []; };
	const runListeners = async (...args) => { for (const l of listeners) await l(...args); }
	return { register, done, fn: (...args) => fn(runListeners, ...args) };
}

function toImageData(a, name) {
	if (a.shape.length !== 2 && a.shape.length !== 3) return { code: tinyapl.errors.rank, message: `${name} expects arrays of rank 2 or 3` };
	const els = a.shape.length === 2 ? 1 : a.shape.at(-1);
	if (![1, 2, 3, 4].includes(els)) return { code: tinyapl.errors.length, message: `${name}: third axis must have length 1, 2, 3 or 4` };
	const data = new ImageData(a.shape[1], a.shape[0]);
	for (let y = 0; y < a.shape[0]; y++)
		for (let x = 0; x < a.shape[1]; x++) {
			const dIdx = (x * a.shape[0] + y) * 4;
			const uIdx = (x * a.shape[0] + y) * els;
			if (els === 1) {
				data.data[dIdx + 0] = data.data[dIdx + 1] = data.data[dIdx + 2] = a.contents[uIdx + 0][0];
				data.data[dIdx + 3] = 255;
			} else if (els === 2) {
				data.data[dIdx + 0] = data.data[dIdx + 1] = data.data[dIdx + 2] = a.contents[uIdx + 0][0];
				data.data[dIdx + 3] = a.contents[uIdx + 1][0];
			} else if (els === 3) {
				data.data[dIdx + 0] = a.contents[uIdx + 0][0];
				data.data[dIdx + 1] = a.contents[uIdx + 1][0];
				data.data[dIdx + 2] = a.contents[uIdx + 2][0];
				data.data[dIdx + 3] = 255;
			} else {
				data.data[dIdx + 0] = a.contents[uIdx + 0][0];
				data.data[dIdx + 1] = a.contents[uIdx + 1][0];
				data.data[dIdx + 2] = a.contents[uIdx + 2][0];
				data.data[dIdx + 3] = a.contents[uIdx + 3][0];
			}
		}
	return data;
}

let windowId = 0;

export const { register: rCreateImage, done: dCreateImage, fn: qCreateImage } = makeFunction(async (runListeners, x, y) => {
	if (y) return { code: tinyapl.errors.domain, message: '⎕CreateImage must be called monadically' };
	if (x.shape.length !== 0 && x.shape.length !== 1) return { code: tinyapl.errors.rank, message: '⎕CreateImage expects arrays of rank 0 or 1' };
	if (x.shape.length === 1 && x.contents.length !== 2) return { code: tinyapl.errors.length, message: '⎕CreateImage expects a scalar or a two-element vector' };
	const height = x.contents[0][0]
	const width = x.contents[x.shape.length][0];
	const id = ++windowId;
	try {
		await runListeners(id, width, height);
	} catch (ex) {
		console.error(ex);
		return { code: tinyapl.errors.user, message: ex.message };
	}
	return { shape: [], contents: [id] };
});

export const { register: rDisplayImage, done: dDisplayImage, fn: qDisplayImage } = makeFunction(async (runListeners, x, y) => {
	let id, a;
	if (y) {
		a = y;
		if (x.shape.length !== 0) return { code: tinyapl.errors.rank, message: '⎕DispayImage left argument must be a scalar natural' };
		id = x.contents[0][0];
	} else a = x;
	const data = toImageData(a, '⎕DisplayImage');
	if ('code' in data) return data;
	try {
		await runListeners(id, data);
	} catch (ex) {
		console.error(ex);
		return { code: tinyapl.errors.user, message: ex.message };
	}
	return { shape: [0], contents: [] };
});

