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
/**
 * Create a new context for TinyAPL code
 * @param input Function providing standard input
 * @param output Function providing standard output
 * @param error Function providing standard error
 * @param quads Quad names available to the interpreter
 */
export declare function newContext(input: () => PromiseLike<string>, output: (what: string) => PromiseLike<void>, error: (what: string) => PromiseLike<void>, quads: Quads): Promise<number>;
/**
 * Run code in a context
 * @param context Context ID
 * @returns A pair containing the result of the code or the error and whether running succeeded
 */
export declare function runCode(context: number, code: string): Promise<[string, boolean]>;
/**
 * List of all global names
 * @param context Context ID
 */
export declare function getGlobals(context: number): Promise<string[]>;
/**
 * Access a global by name
 * @param context Context ID
 */
export declare function getGlobal(context: number, name: string): Promise<Err | Arr | Fun>;
/**
 * Set a global by name
 * @param context Context ID
 */
export declare function setGlobal(context: number, name: string, val: Arr | Fun): Promise<Err | void>;
/**
 * Higlight a piece of code
 */
export declare function highlight(code: string): Promise<number[]>;
/**
 * Split a string into UTF32 codepoints
 */
export declare function splitString(str: string): Promise<string[]>;
/**
 * Join a string of UTF32 codepoints
 */
export declare function joinString(strs: string[]): Promise<string>;
export declare const glyphs: {
    syntax: any;
    identifiers: any;
    arrays: any;
    functions: any;
    adverbs: any;
    conjunctions: any;
};
export declare const colors: Record<string, number>;
export declare const colorsInv: Record<number, string>;
export declare const errors: Record<string, number>;
