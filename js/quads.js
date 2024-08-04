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
			const dIdx = (y * a.shape[0] + x) * 4;
			const uIdx = (y * a.shape[0] + x) * els;
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

export const { register: rDisplayImage, done: dDisplayImage, fn: qDisplayImage } = makeFunction(async (runListeners, a, y) => {
	if (y) return { code: tinyapl.errors.domain, message: '⎕DisplayImage must be called monadically' };
	const data = toImageData(a, '⎕DisplayImage');
	if ('code' in data) return data;
	try {
		await runListeners(data);
	} catch (ex) {
		console.error(ex);
		return { code: tinyapl.errors.user, message: ex.message };
	}
	return { shape: [0], contents: [] };
});

const windows = [];

export const { register: rCreateWindow, done: dCreateWindow, fn: qCreateWindow } = makeFunction(async (runListeners, a, y) => {
	if (y) return { code: tinyapl.errors.domain, message: '⎕CreateWindow must be called monadically' };
	if (a.shape.length !== 0 && a.shape.length !== 1) return { code: tinyapl.errors.rank, message: '⎕CreateWindow expects arrays of rank 0 or 1' };
	if (a.shape.length === 1 && a.contents.length !== 2) return { code: tinyapl.errors.length, message: '⎕CreateWindow expects a scalar or a two-element vector' };
	const height = a.contents[0][0]
	const width = a.contents[a.shape.length][0];
	const id = windows.length;
	const win = window.open('', '_blank', `popup=true,innerWidth=${width},innerHeight=${height}`);
	win.document.body.innerHTML = '';
	win.document.body.style.margin = 0;
	const canvas = win.document.createElement('canvas');
	canvas.width = width;
	canvas.height = height;
	canvas.id = 'canvas';
	win.document.body.appendChild(canvas);
	win.addEventListener('beforeunload', () => { windows[id] = null; });
	windows.push(win);
	try {
		await runListeners(win, canvas);
	} catch (ex) {
		console.error(ex);
		return { code: tinyapl.errors.user, message: ex.message };
	}
	return { shape: [], contents: [[id, 0]] };
});

export const { register: rUpdateWindow, done: dUpdateWindow, fn: qUpdateWindow } = makeFunction(async (runListeners, id, a) => {
	console.log('UpdateWindow', id, a);
	if (!a) return { code: tinyapl.errors.domain, message: '⎕UpdateWindow must be called dyadically' };
	if (id.shape.length !== 0) return { code: tinyapl.errors.rank, message: '⎕UpdateWindow left argument must be a scalar containing the ID of a window' };
	const win = windows[id.contents[0][0]];
	if (win === undefined) return { code: tinyapl.errors.domain, message: '⎕UpdateWindow could not find a window with provided ID' };
	if (win === null) return { code: tinyapl.errors.domain, message: '⎕UpdateWindow window was closed' };
	const data = toImageData(a, '⎕UpdateWindow');
	if ('code' in data) return data;
	const canvas = win.document.querySelector('#canvas');
	console.log(canvas);
	const ctx = canvas.getContext('2d');
	console.log(ctx);
	ctx.clearRect(0, 0, canvas.width, canvas.height);
	ctx.putImageData(data, 0, 0);
	console.log(data);
	try {
		await runListeners(win, canvas, data);
	} catch (ex) {
		console.error(ex);
		return { code: tinyapl.errors.user, message: ex.message };
	}
	return { shape: [0], contents: [] };
});

export const { register: rDeleteWindow, done: dDeleteWindow, fn: qDeleteWindow } = makeFunction(async (runListeners, id, y) => {
	if (y) return { code: tinyapl.errors.domain, message: '⎕DeleteWindow must be called monadically' };
	if (id.shape.length !== 0) return { code: tinyapl.errors.rank, message: '⎕DeleteWindow argument must be a scalar containing the ID of a window' };
	const win = windows[id.contents[0][0]];
	if (win === undefined) return { code: tinyapl.errors.domain, message: '⎕DeleteWindow could not find a window with provided ID' };
	if (win === null) return { code: tinyapl.errors.domain, message: '⎕DeleteWindow window was already closed' };
	windows[id.contents[0][0]].close();
	windows[id.contents[0][0]] = null;
	try {
		await runListeners();
	} catch (ex) {
		console.error(ex);
		return { code: tinyapl.errors.user, message: ex.message };
	}
	return { shape: [0], contents: [] };
});

window.addEventListener('beforeunload', () => { for (const win of windows) if (win !== null && win !== undefined) { try { win.close(); } catch {} } });
