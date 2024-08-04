import { WASI, OpenFile, File, ConsoleStdout } from 'https://esm.run/@bjorn3/browser_wasi_shim@0.3.0';
import ghc_wasm_jsffi from './ghc_wasm_jsffi.js';

/** @typedef {[number, number] | string | Arr} ScalarValue */
/** @typedef {{ shape: number[], contents: ScalarValue[] }} Arr */
/** @typedef {{ code: number, message: string }} Err */

const args = [];
const env = [];
const files = [
	new OpenFile(new File([])), // stdin
	ConsoleStdout.lineBuffered(msg => console.log(`[WASI] ${msg}`)), // stdout
	ConsoleStdout.lineBuffered(msg => console.warn(`[WASI] ${msg}`)), // stderr
];
const options = {};
const wasi = new WASI(args, env, files, options);

const instanceExports = {};
const { instance } = await WebAssembly.instantiateStreaming(fetch("./tinyapl-js.wasm"), {
	wasi_snapshot_preview1: wasi.wasiImport,
	ghc_wasm_jsffi: ghc_wasm_jsffi(instanceExports),
});
Object.assign(instanceExports, instance.exports);

wasi.initialize(instance);

await instance.exports.hs_start();

/**
 * Create a new context for TinyAPL code
 * @param {() => string | Promise<string>} input Function providing standard input
 * @param {(what: string) => void | Promise<void>} output Function providing standard output
 * @param {(what: string) => void | Promise<void>} error Function providing standard error
 * @param {Record<string, 
 *   ((() => Arr | Err | Promise<Arr | Err>) & ((set: Arr) => void | Err | Promise<void | Err>))
 * | (((y: Arr) => Arr | Err | Promise<Arr | Err>) & ((x: Arr, y: Arr) => Arr | Err | Promise<Arr | Err>))} quads Quad names available to the interpreter
 */
export async function newContext(input, output, error, quads) {
	return await instance.exports.tinyapl_newContext(input, output, error, quads);
}

/**
 * Run code in a context
 * @param {number} context Context ID
 * @param {string} code
 * @returns {Promise<[string, boolean]>} A pair containing the result of the code or the error and whether running succeeded
 */
export async function runCode(context, code) {
	const [result, success] = await instance.exports.tinyapl_runCode(context, code);
	return [await joinString(result), Boolean(success)];
}

/**
 * Higlight a piece of code
 * @param {string} code
 * @returns {Promise<number[]>} Colors of each character
 */
export async function highlight(code) {
	return await instance.exports.tinyapl_highlight(code);
}

/**
 * Split a string into UTF32 codepoints
 * @param {string} str
 * @returns {Promise<string[]>}
 */
export async function splitString(str) {
	return await instance.exports.tinyapl_splitString(str);
}

/**
 * Join a string of UTF32 codepoints
 * @param {string[]} strs
 * @returns {Promise<string[]>}
 */
export async function joinString(strs) {
	return await instance.exports.tinyapl_joinString(strs);
}

/**
 * @type {Record<string, string[]>}
 */
export const glyphs = {
	syntax: await instance.exports.tinyapl_glyphsSyntax(),
	identifiers: await instance.exports.tinyapl_glyphsIdentifiers(),
	arrays: await instance.exports.tinyapl_glyphsArrays(),
	functions: await instance.exports.tinyapl_glyphsFunctions(),
	adverbs: await instance.exports.tinyapl_glyphsAdverbs(),
	conjunctions: await instance.exports.tinyapl_glyphsConjunctions(),
};

/**
 * @type {Record<string, number>}
 */
export const colors = Object.fromEntries(await Promise.all(Object.entries(instance.exports).filter(([k]) => k.startsWith('tinyapl_hl')).map(async ([k, v]) => [k['tinyapl_hl'.length].toLowerCase() + k.slice('tinyapl_hl'.length + 1), await v()])));

/**
 * @type {Record<number, string>}
 */
export const colorsInv = Object.fromEntries(Object.entries(colors).map(([k, v]) => [v, k]));

/**
 * @type {Record<string, number>}
 */
export const errors = Object.fromEntries(await Promise.all(Object.entries(instance.exports).filter(([k]) => k.startsWith('tinyapl_err')).map(async ([k, v]) => [k['tinyapl_err'.length].toLowerCase() + k.slice('tinyapl_err'.length + 1), await v()])));

