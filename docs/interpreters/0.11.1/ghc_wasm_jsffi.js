// This file implements the JavaScript runtime logic for Haskell
// modules that use JSFFI. It is not an ESM module, but the template
// of one; the post-linker script will copy all contents into a new
// ESM module.
// Manage a mapping from unique 32-bit ids to actual JavaScript
// values.
class JSValManager {
    #lastk = 0;
    #kv = new Map();
    constructor() { }
    // Maybe just bump this.#lastk? For 64-bit ids that's sufficient,
    // but better safe than sorry in the 32-bit case.
    #allocKey() {
        let k = this.#lastk;
        while (true) {
            if (!this.#kv.has(k)) {
                this.#lastk = k;
                return k;
            }
            k = (k + 1) | 0;
        }
    }
    newJSVal(v) {
        const k = this.#allocKey();
        this.#kv.set(k, v);
        return k;
    }
    // A separate has() call to ensure we can store undefined as a value
    // too. Also, unconditionally check this since the check is cheap
    // anyway, if the check fails then there's a use-after-free to be
    // fixed.
    getJSVal(k) {
        if (!this.#kv.has(k)) {
            throw new WebAssembly.RuntimeError(`getJSVal(${k})`);
        }
        return this.#kv.get(k);
    }
    // Check for double free as well.
    freeJSVal(k) {
        if (!this.#kv.delete(k)) {
            throw new WebAssembly.RuntimeError(`freeJSVal(${k})`);
        }
    }
}
// A simple & fast setImmediate() implementation for browsers. It's
// not a drop-in replacement for node.js setImmediate() because:
// 1. There's no clearImmediate(), and setImmediate() doesn't return
//    anything
// 2. There's no guarantee that callbacks scheduled by setImmediate()
//    are executed in the same order (in fact it's the opposite lol),
//    but you are never supposed to rely on this assumption anyway
class SetImmediate {
    #fs = [];
    #mc = new MessageChannel();
    constructor() {
        this.#mc.port1.addEventListener("message", () => {
            this.#fs.pop()();
        });
        this.#mc.port1.start();
    }
    setImmediate(cb, ...args) {
        this.#fs.push(() => cb(...args));
        this.#mc.port2.postMessage(undefined);
    }
}
// The actual setImmediate() to be used. This is a ESM module top
// level binding and doesn't pollute the globalThis namespace.
let setImmediate;
if (globalThis.setImmediate) {
    // node.js, bun
    setImmediate = globalThis.setImmediate;
}
else {
    try {
        // deno
        setImmediate = (await import("node:timers")).setImmediate;
    }
    catch {
        // browsers
        const sm = new SetImmediate();
        setImmediate = (cb, ...args) => sm.setImmediate(cb, ...args);
    }
}
export default (__exports) => {
    const __ghc_wasm_jsffi_jsval_manager = new JSValManager();
    const __ghc_wasm_jsffi_finalization_registry = new FinalizationRegistry(sp => __exports.rts_freeStablePtr(sp));
    return {
        newJSVal: (v) => __ghc_wasm_jsffi_jsval_manager.newJSVal(v),
        getJSVal: (k) => __ghc_wasm_jsffi_jsval_manager.getJSVal(k),
        freeJSVal: (k) => __ghc_wasm_jsffi_jsval_manager.freeJSVal(k),
        scheduleWork: () => setImmediate(__exports.rts_schedulerLoop),
        ZC1ZCtinyaplzm0zi11zi1zi0zminplacezmtinyaplzmjsZCJSBridgeZC: ($1) => ((a1, a2) => __exports.ghczuwasmzujsffiZC0ZCtinyaplzm0zi11zi1zi0zminplacezmtinyaplzmjsZCJSBridgeZC($1, a1, a2)),
        ZC3ZCtinyaplzm0zi11zi1zi0zminplacezmtinyaplzmjsZCJSBridgeZC: ($1) => ((a1) => __exports.ghczuwasmzujsffiZC2ZCtinyaplzm0zi11zi1zi0zminplacezmtinyaplzmjsZCJSBridgeZC($1, a1)),
        ZC6ZCtinyaplzm0zi11zi1zi0zminplacezmtinyaplzmjsZCJSBridgeZC: async ($1, $2, $3) => { return await $1($2, $3); },
        ZC7ZCtinyaplzm0zi11zi1zi0zminplacezmtinyaplzmjsZCJSBridgeZC: async ($1, $2) => { return await $1($2); },
        ZC8ZCtinyaplzm0zi11zi1zi0zminplacezmtinyaplzmjsZCJSBridgeZC: async ($1) => { return await $1(); },
        ZC9ZCtinyaplzm0zi11zi1zi0zminplacezmtinyaplzmjsZCJSBridgeZC: ($1, $2) => { return $1 in $2; },
        ZC10ZCtinyaplzm0zi11zi1zi0zminplacezmtinyaplzmjsZCJSBridgeZC: ($1, $2) => { return $1[$2]; },
        ZC11ZCtinyaplzm0zi11zi1zi0zminplacezmtinyaplzmjsZCJSBridgeZC: ($1) => { return Array.isArray($1); },
        ZC12ZCtinyaplzm0zi11zi1zi0zminplacezmtinyaplzmjsZCJSBridgeZC: ($1) => { return typeof $1; },
        ZC13ZCtinyaplzm0zi11zi1zi0zminplacezmtinyaplzmjsZCJSBridgeZC: ($1) => { return Object.fromEntries($1); },
        ZC14ZCtinyaplzm0zi11zi1zi0zminplacezmtinyaplzmjsZCJSBridgeZC: ($1) => { return Object.entries($1); },
        ZC15ZCtinyaplzm0zi11zi1zi0zminplacezmtinyaplzmjsZCJSBridgeZC: ($1) => { return $1[1]; },
        ZC16ZCtinyaplzm0zi11zi1zi0zminplacezmtinyaplzmjsZCJSBridgeZC: ($1) => { return $1[0]; },
        ZC17ZCtinyaplzm0zi11zi1zi0zminplacezmtinyaplzmjsZCJSBridgeZC: ($1, $2) => { return [$1, $2]; },
        ZC20ZCtinyaplzm0zi11zi1zi0zminplacezmtinyaplzmjsZCJSBridgeZC: ($1) => { return $1; },
        ZC21ZCtinyaplzm0zi11zi1zi0zminplacezmtinyaplzmjsZCJSBridgeZC: ($1) => { return $1; },
        ZC22ZCtinyaplzm0zi11zi1zi0zminplacezmtinyaplzmjsZCJSBridgeZC: ($1) => { return $1; },
        ZC23ZCtinyaplzm0zi11zi1zi0zminplacezmtinyaplzmjsZCJSBridgeZC: ($1) => { return $1; },
        ZC24ZCtinyaplzm0zi11zi1zi0zminplacezmtinyaplzmjsZCJSBridgeZC: ($1) => { return $1 === undefined; },
        ZC25ZCtinyaplzm0zi11zi1zi0zminplacezmtinyaplzmjsZCJSBridgeZC: () => { return undefined; },
        ZC27ZCtinyaplzm0zi11zi1zi0zminplacezmtinyaplzmjsZCJSBridgeZC: ($1, $2) => { $1.push($2); return $1; },
        ZC28ZCtinyaplzm0zi11zi1zi0zminplacezmtinyaplzmjsZCJSBridgeZC: ($1, $2) => { return $1[$2]; },
        ZC29ZCtinyaplzm0zi11zi1zi0zminplacezmtinyaplzmjsZCJSBridgeZC: ($1) => { return $1.length; },
        ZC33ZCtinyaplzm0zi11zi1zi0zminplacezmtinyaplzmjsZCJSBridgeZC: ($1) => { return []; },
        ZC34ZCtinyaplzm0zi11zi1zi0zminplacezmtinyaplzmjsZCJSBridgeZC: ($1) => { return $1; },
        ZC35ZCtinyaplzm0zi11zi1zi0zminplacezmtinyaplzmjsZCJSBridgeZC: ($1) => { return $1; },
        ZC39ZCtinyaplzm0zi11zi1zi0zminplacezmtinyaplzmjsZCMainZC: async ($1, $2) => { await $1($2); },
        ZC40ZCtinyaplzm0zi11zi1zi0zminplacezmtinyaplzmjsZCMainZC: async ($1) => { return await $1(); },
        ZC41ZCtinyaplzm0zi11zi1zi0zminplacezmtinyaplzmjsZCMainZC: async ($1) => { return await fetch($1).then(x => x.text()); },
        ZC0ZCghczminternalZCGHCziInternalziWasmziPrimziExportsZC: ($1, $2) => ($1.reject(new WebAssembly.RuntimeError($2))),
        ZC16ZCghczminternalZCGHCziInternalziWasmziPrimziExportsZC: ($1, $2) => ($1.resolve($2)),
        ZC18ZCghczminternalZCGHCziInternalziWasmziPrimziExportsZC: ($1, $2) => ($1.resolve($2)),
        ZC19ZCghczminternalZCGHCziInternalziWasmziPrimziExportsZC: ($1) => ($1.resolve()),
        ZC20ZCghczminternalZCGHCziInternalziWasmziPrimziExportsZC: () => { let res, rej; const p = new Promise((resolve, reject) => { res = resolve; rej = reject; }); p.resolve = res; p.reject = rej; return p; },
        ZC21ZCghczminternalZCGHCziInternalziWasmziPrimziExportsZC: ($1, $2) => (__ghc_wasm_jsffi_finalization_registry.register($1, $2, $1)),
        ZC17ZCghczminternalZCGHCziInternalziWasmziPrimziImportsZC: ($1, $2) => ($1.then(res => __exports.rts_promiseResolveJSVal($2, res), err => __exports.rts_promiseReject($2, err))),
        ZC18ZCghczminternalZCGHCziInternalziWasmziPrimziImportsZC: ($1, $2) => ($1.then(() => __exports.rts_promiseResolveUnit($2), err => __exports.rts_promiseReject($2, err))),
        ZC0ZCghczminternalZCGHCziInternalziWasmziPrimziTypesZC: ($1) => (`${$1.stack ? $1.stack : $1}`),
        ZC1ZCghczminternalZCGHCziInternalziWasmziPrimziTypesZC: ($1, $2) => ((new TextDecoder('utf-8', { fatal: true })).decode(new Uint8Array(__exports.memory.buffer, $1, $2))),
        ZC2ZCghczminternalZCGHCziInternalziWasmziPrimziTypesZC: ($1, $2, $3) => ((new TextEncoder()).encodeInto($1, new Uint8Array(__exports.memory.buffer, $2, $3)).written),
        ZC3ZCghczminternalZCGHCziInternalziWasmziPrimziTypesZC: ($1) => ($1.length),
        ZC4ZCghczminternalZCGHCziInternalziWasmziPrimziTypesZC: ($1) => { if (!__ghc_wasm_jsffi_finalization_registry.unregister($1)) {
            throw new WebAssembly.RuntimeError('js_callback_unregister');
        } },
        ZC0ZCghczminternalZCGHCziInternalziWasmziPrimziConcziInternalZC: async ($1) => (new Promise(res => setTimeout(res, $1 / 1000))),
    };
};
