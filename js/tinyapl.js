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