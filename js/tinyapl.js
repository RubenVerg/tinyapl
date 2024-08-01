import { WASI, OpenFile, File, ConsoleStdout } from 'https://esm.run/@bjorn3/browser_wasi_shim@0.3.0';
import ghc_wasm_jsffi from './ghc_wasm_jsffi.js';

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

const { tinyapl_newScope, tinyapl_runCode } = instance.exports;

/**
 * Create a new scope for TinyAPL code
 * @returns {number} Scope ID
 */
export async function newScope() {
	return await tinyapl_newScope();
}

/**
 * Run code in a scope
 * @param {number} scope Scope ID
 * @param {string} code
 * @returns {[string, boolean]} A pair containing the result of the code or the error and whether running succeeded
 */
export async function runCode(scope, code) {
	const [result, success] = await tinyapl_runCode(scope, code);
	return [result, Boolean(success)];
}

