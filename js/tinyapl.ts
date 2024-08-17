// @ts-ignore Import from web not supported
import { WASI, OpenFile, File, ConsoleStdout } from 'https://esm.run/@bjorn3/browser_wasi_shim@0.3.0';
import ghc_wasm_jsffi from './ghc_wasm_jsffi.js';

declare global {
	interface ImportMeta {
		resolve(specifier: string): string;
	}
}

export type Complex = [number, number];
export type ScalarValue = Complex | string | Arr;
export interface Arr {
	type: 'array';
	shape: number[];
	contents: ScalarValue[];
}
export interface Fun {
	type: 'function';
	repr: string;
	monad(y: Arr): PromiseLike<Err | Arr>;
	dyad(x: Arr, y: Arr): PromiseLike<Err | Arr>;
}
export interface Err {
	code: number;
	message: string;
}
export type NiladGet = () => PromiseLike<Err | Arr>;
export type NiladSet = (arr: Arr) => PromiseLike<Err | void>;
export type Monad = (y: Arr) => PromiseLike<Err | Arr>;
export type Dyad = (x: Arr, y: Arr) => PromiseLike<Err | Arr>;
export type Quads = Record<string, (NiladGet & NiladSet) | (Monad & Dyad)>;

const files = [
	new OpenFile(new File([], {})), // stdin
	ConsoleStdout.lineBuffered((msg: string) => console.log(`[WASI] ${msg}`)), // stdout
	ConsoleStdout.lineBuffered((msg: string) => console.warn(`[WASI] ${msg}`)), // stderr
];
const options = {};
const wasi = new WASI([], [], files, options);

const instanceExports = {};
const url = 'resolve' in import.meta ? import.meta.resolve('./tinyapl-js.wasm') : './tinyapl-js.wasm';
const { instance } = await WebAssembly.instantiateStreaming(fetch(url), {
	wasi_snapshot_preview1: (wasi as any).wasiImport,
	ghc_wasm_jsffi: ghc_wasm_jsffi(instanceExports),
});
Object.assign(instanceExports, instance.exports);

wasi.initialize(instance);

const exports = instance.exports as any;

await exports.hs_start();

/**
 * Create a new context for TinyAPL code
 * @param input Function providing standard input
 * @param output Function providing standard output
 * @param error Function providing standard error
 * @param quads Quad names available to the interpreter
 */
export async function newContext(input: () => PromiseLike<string>, output: (what: string) => PromiseLike<void>, error: (what: string) => PromiseLike<void>, quads: Quads): Promise<number> {
	return await exports.tinyapl_newContext(input, output, error, quads);
}

/**
 * Run code in a context
 * @param context Context ID
 * @returns A pair containing the result of the code or the error and whether running succeeded
 */
export async function runCode(context: number, code: string): Promise<[string, boolean]> {
	const [result, success] = await exports.tinyapl_runCode(context, code);
	return [await joinString(result), Boolean(success)];
}

/**
 * List of all global names
 * @param context Context ID
 */
export async function getGlobals(context: number): Promise<string[]> {
	return await exports.tinyapl_getGlobals(context);
}

/**
 * Access a global by name
 * @param context Context ID
 */
export async function getGlobal(context: number, name: string): Promise<Err | Arr | Fun> {
	return await exports.tinyapl_getGlobal(context, name);
}

/**
 * Set a global by name
 * @param context Context ID
 */
export async function setGlobal(context: number, name: string, val: Arr | Fun): Promise<Err | void> {
	return await exports.tinyapl_setGlobal(context, name, val);
}

/**
 * Higlight a piece of code
 */
export async function highlight(code: string): Promise<number[]> {
	return await exports.tinyapl_highlight(code);
}

/**
 * Split a string into UTF32 codepoints
 */
export async function splitString(str: string): Promise<string[]> {
	return await exports.tinyapl_splitString(str);
}

/**
 * Join a string of UTF32 codepoints
 */
export async function joinString(strs: string[]): Promise<string> {
	return await exports.tinyapl_joinString(strs);
}

export const glyphs = {
	syntax: await exports.tinyapl_glyphsSyntax(),
	identifiers: await exports.tinyapl_glyphsIdentifiers(),
	arrays: await exports.tinyapl_glyphsArrays(),
	functions: await exports.tinyapl_glyphsFunctions(),
	adverbs: await exports.tinyapl_glyphsAdverbs(),
	conjunctions: await exports.tinyapl_glyphsConjunctions(),
};

export const colors: Record<string, number> = Object.fromEntries(await Promise.all(Object.entries(instance.exports).filter(([k]) => k.startsWith('tinyapl_hl')).map(async ([k, v]) => [k['tinyapl_hl'.length].toLowerCase() + k.slice('tinyapl_hl'.length + 1), await (v as () => Promise<number>)()])));

export const colorsInv: Record<number, string> = Object.fromEntries(Object.entries(colors).map(([k, v]) => [v, k]));

export const errors: Record<string, number> = Object.fromEntries(await Promise.all(Object.entries(instance.exports).filter(([k]) => k.startsWith('tinyapl_err')).map(async ([k, v]) => [k['tinyapl_err'.length].toLowerCase() + k.slice('tinyapl_err'.length + 1), await (v as () => Promise<number>)()])));

