import * as tinyapl from './tinyapl.js';
export declare const rCreateImage: (l: (args_0: number, args_1: number, args_2: number) => Promise<void>) => void, dCreateImage: () => void, qCreateImage: (args_0: tinyapl.Arr) => Promise<tinyapl.Arr | tinyapl.Err>;
export declare const rDisplayImage: (l: (args_0: number | undefined, args_1: ImageData) => Promise<void>) => void, dDisplayImage: () => void, qDisplayImage: (...args: [tinyapl.Arr] | [tinyapl.Arr, tinyapl.Arr]) => Promise<tinyapl.Arr | tinyapl.Err>;
export declare const rPlayAnimation: (l: (args_0: number, args_1: ImageData[]) => Promise<void>) => void, dPlayAnimation: () => void, qPlayAnimation: (...args: [tinyapl.Arr] | [tinyapl.Arr, tinyapl.Arr]) => Promise<tinyapl.Arr | tinyapl.Err>;
export declare const rScatterPlot: (l: (args_0: number[][], args_1: number[][], args_2: string) => Promise<void>) => void, dScatterPlot: () => void, qScatterPlot: (...args: [tinyapl.Arr] | [tinyapl.Arr, tinyapl.Arr]) => Promise<tinyapl.Arr | tinyapl.Err>;
export declare const rPlayAudio: (l: (args_0: ArrayBuffer) => Promise<void>) => void, dPlayAudio: () => void, qPlayAudio: (...args: [tinyapl.Arr] | [tinyapl.Arr, tinyapl.Arr]) => Promise<tinyapl.Arr | tinyapl.Err>;
export declare function qFetch(x: tinyapl.Arr, y?: tinyapl.Arr): Promise<tinyapl.Arr | tinyapl.Err>;
