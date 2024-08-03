import * as tinyapl from './tinyapl.js';

let lDisplayImage = [];

export function rDisplayImage(listener) {
	lDisplayImage.push(listener);
}

export function dDisplayImage() {
	lDisplayImage = [];
}

export async function qDisplayImage(/** @type {import('./tinyapl.js').Arr} */ a, y) {
	if (y) return { code: tinyapl.errors.domain, message: '⎕DisplayImage must be called monadically' };
	if (a.shape.length !== 2 && a.shape.length !== 3) return { code: tinyapl.errors.rank, message: '⎕DisplayImage expects arrays of rank 2 or 3' };
	const els = a.shape.length === 2 ? 1 : a.shape.at(-1);
	if (![1, 2, 3, 4].includes(els)) return { code: tinyapl.errors.length, message: '⎕DisplayImage: third axis must have length 1, 2, 3 or 4' };
	const data = new ImageData(a.shape[1], a.shape[0]);
	for (let y = 0; y < a.shape[0]; y++)
		for (let x = 0; x < a.shape[1]; x++) {
			const idx = y * a.shape[0] + x;
			if (els === 1) {
				data.data[idx + 0] = data.data[idx + 1] = data.data[idx + 2] = a.contents[idx + 0][0];
				data.data[idx + 3] = 255;
			} else if (els === 2) {
				data.data[idx + 0] = data.data[idx + 1] = data.data[idx + 2] = a.contents[idx + 0][0];
				data.data[idx + 3] = a.contents[idx + 1][0];
			} else if (els === 3) {
				data.data[idx + 0] = a.contents[idx + 0][0];
				data.data[idx + 1] = a.contents[idx + 1][0];
				data.data[idx + 2] = a.contents[idx + 2][0];
				data.data[idx + 3] = 255;
			} else {
				data.data[idx + 0] = a.contents[idx + 0][0];
				data.data[idx + 1] = a.contents[idx + 1][0];
				data.data[idx + 2] = a.contents[idx + 2][0];
				data.data[idx + 3] = a.contents[idx + 3][0];
			}
		}
	for (const listener of lDisplayImage)
		try {
			await listener();
		} catch (ex) {
			return { code: tinyapl.errors.user, message: ex.message };
		}
	return { shape: [0], contents: [] };
}
