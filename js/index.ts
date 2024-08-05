import * as tinyapl from './tinyapl.js';
import * as quads from './quads.js';

import 'https://cdn.plot.ly/plotly-2.34.0.min.js';

const buttons = document.querySelector<HTMLDivElement>('#buttons')!;
const output = document.querySelector<HTMLPreElement>('#output')!;
const input = document.querySelector<HTMLInputElement>('#input')!;
const highlighted = document.querySelector<HTMLPreElement>('#highlighted')!;
const button = document.querySelector<HTMLButtonElement>('#button')!;

function zip<A, B>(as: A[], bs: B[]): [A, B][] {
	return [...as, ...bs].slice(0, Math.min(as.length, bs.length)).map((_, idx) => [as[idx], bs[idx]]);
}

const prefix = { code: 'Backquote', sym: '`' };

const keyboard = [
	['Backquote', '`', '~', undefined, '⍨', '⋄', '⌺'],
	['Digit1', '1', '!', '¨', undefined, undefined, undefined],
	['Digit2', '2', '@', '¯', undefined, undefined, undefined],
	['Digit3', '3', '#', undefined, '⍒', undefined, undefined],
	['Digit4', '4', '$', '≤', '⍋', '⊴', undefined],
	['Digit5', '5', '%', undefined, undefined, undefined, undefined],
	['Digit6', '6', '^', '≥', '⍉', '⊵', undefined],
	['Digit7', '7', '&', undefined, '⊖', undefined, undefined],
	['Digit8', '8', '*', '≠', '⍣', '⍟', '∞'],
	['Digit9', '9', '(', '∨', '⍱', undefined, undefined],
	['Digit0', '0', ')', '∧', '⍲', '⍬', undefined],
	['Minus', '-', '_', '×', '⊗', '⸚', undefined],
	['Equal', '=', '+', '÷', '⊕', '⌹', undefined],
	['KeyQ', 'q', 'Q', undefined, undefined, undefined, undefined],
	['KeyW', 'w', 'W', '⍵', '⍹', undefined, undefined],
	['KeyE', 'e', 'E', '∊', '⍷', '⏨', '⋷'],
	['KeyR', 'r', 'R', '⍴', '√', 'ϼ', 'ℜ'],
	['KeyT', 't', 'T', '⊞', '⍨', undefined, undefined],
	['KeyY', 'y', 'Y', '↑', '↟', undefined, undefined],
	['KeyU', 'u', 'U', '↓', '↡', undefined, undefined],
	['KeyI', 'i', 'I', '⍳', '⍸', '…', 'ℑ'],
	['KeyO', 'o', 'O', '○', '⍥', undefined, undefined],
	['KeyP', 'p', 'P', undefined, undefined, undefined, undefined],
	['BracketLeft', '[', '{', '←', '⟨', undefined, undefined],
	['BracketRight', ']', '}', undefined, '⟩', undefined, undefined],
	['KeyA', 'a', 'A', '⍺', '⍶', undefined, undefined],
	['KeyS', 's', 'S', '⌈', '§', '↾', undefined],
	['KeyD', 'd', 'D', '⌊', '⸠', '⇂', undefined],
	['KeyF', 'f', 'F', '⍛', '∡', '∠', undefined],
	['KeyG', 'g', 'G', '∇', '⍢', undefined, undefined],
	['KeyH', 'h', 'H', '∆', '⍙', '⊸', '⟜'],
	['KeyJ', 'j', 'J', '∘', '⍤', 'ᴊ', undefined],
	['KeyK', 'k', 'K', '⍆', '⌸', '⍅', undefined],
	['KeyL', 'l', 'L', '⎕', '⌷', undefined, undefined],
	['Semicolon', ';', ':', '⍎', '≡', '⍮', '■'],
	['Quote', '\'', '"', '⍕', '≢', '⍘', '⍞'],
	['Backslash', '\\', '|', '⊢', '⊣', undefined, undefined],
	['KeyZ', 'z', 'Z', '⊂', '⊆', undefined, undefined],
	['KeyX', 'x', 'X', '⊃', '⊇', '↗', undefined],
	['KeyC', 'c', 'C', '∩', '⍝', '⟃', '⟄'],
	['KeyV', 'v', 'V', '∪', '⁖', undefined, undefined],
	['KeyB', 'b', 'B', '⊥', undefined, undefined, undefined],
	['KeyN', 'n', 'N', '⊤', undefined, undefined, undefined],
	['KeyM', 'm', 'M', '«', '»', undefined, undefined],
	['Comma', ',', '<', '⍪', 'ᑈ', '⊲', undefined],
	['Period', '.', '>', '∙', 'ᐵ', '⊳', undefined],
	['Slash', '/', '?', '⌿', undefined, undefined, undefined],
].map(([code, sym, symS, symP, symPS, symPP, symPPS]) => ({ code, sym, symS, symP, symPS, symPP, symPPS }));

const colors = {
	other: 'unset',
	syntax: 'unset',
	number: '#ea0027',
	string: '#0079d3',
	stringEscape: '#0266b3',
	arrayName: '#ff4500',
	primArray: '#cc3600',
	functionName: '#46d160',
	primFunction: '#349e48',
	adverbName: '#ff66ac',
	primAdverb: '#cc5289',
	conjunctionName: '#ffd635',
	primConjunction: '#ccac2b',
	comment: '#014980',
};

async function highlight() {
	const code = input.value;
	const pairs = zip(await tinyapl.splitString(code), await tinyapl.highlight(code));
	highlighted.innerHTML = '';
	for (const [t, c] of pairs) {
		const span = document.createElement('span');
		span.style.color = colors[tinyapl.colorsInv[c] as keyof typeof colors];
		span.innerText = t;
		highlighted.appendChild(span);
	}
	highlighted.scrollLeft = input.scrollLeft;
}

function insertText(str: string) {
	input.setRangeText(str, input.selectionStart ?? input.value.length - 1, input.selectionEnd ?? input.value.length - 1, "end");
	input.focus();
	highlight();
}

const io = new class IO {
	#input: ((i: string) => PromiseLike<void>)[] = [];
	#output: ((o: string) => PromiseLike<void>)[] = [];
	#error: ((e: string) => PromiseLike<void>)[] = [];
	rInput(l: (i: string) => PromiseLike<void>) { this.#input.push(l); }
	rOutput(l: (o: string) => PromiseLike<void>) { this.#output.push(l); }
	rError(l: (e: string) => PromiseLike<void>) { this.#error.push(l); }
	done() { this.#input = []; this.#output = []; this.#error = []; }
	async input() { const i = window.prompt('Input') ?? ''; for (const l of this.#input) await l(i); return i; }
	async output(what: string) { for (const l of this.#output) await l(what); }
	async error(what: string) { for (const l of this.#error) await l(what); }
};

for (const k of (['syntax', 'identifiers', 'arrays', 'functions', 'adverbs', 'conjunctions'] as const as readonly (keyof typeof tinyapl.glyphs)[])) {
	for (const i of tinyapl.glyphs[k]) {
		let v, p;
		if (v = keyboard.find(k => k.symP === i)) p = `${prefix.sym}${v.sym}`;
		else if (v = keyboard.find(k => k.symPS === i)) p = `${prefix.sym}${v.symS}`;
		else if (v = keyboard.find(k => k.symPP === i)) p = `${prefix.sym}${prefix.sym}${v.sym}`;
		else if (v = keyboard.find(k => k.symPPS === i)) p = `${prefix.sym}${prefix.sym}${v.symS}`;
		const b = document.createElement('button');
		b.textContent = i;
		if (p !== undefined) b.title = `Input: ${p}`;
		b.addEventListener('click', () => { insertText(i); });
		buttons.appendChild(b);
	}
	buttons.appendChild(document.createElement('br'));
}

const context = await tinyapl.newContext(io.input.bind(io), io.output.bind(io), io.error.bind(io), {
	Debug: async (a: tinyapl.Arr, b?: tinyapl.Arr) => { if (b === undefined) { console.log('monad call', a); } else { console.log('dyad call', a, b); } return b ?? a; },
	CreateImage: quads.qCreateImage as (tinyapl.Monad & tinyapl.Dyad),
	DisplayImage: quads.qDisplayImage,
	ScatterPlot: quads.qScatterPlot,
	PlayAudio: quads.qPlayAudio,
	Fetch: quads.qFetch,
});

function div(cls: string, contents: string) {
	const div = document.createElement('div');
	div.className = cls;
	div.textContent = contents;
	return div;
}

function clickableDiv(cls: string, contents: string, clickedContents = contents) {
	const d = div(cls, contents);
	d.addEventListener('click', () => {
		if (input.value.trim() == '') {
			input.value = clickedContents;
			input.focus();
			highlight();
		}
	});
	return d;
}

const images: Record<number, HTMLCanvasElement> = {};

async function runCode(code: string) {
	let d: HTMLDivElement;
	
	const endDiv = () => {
		if (d.textContent!.trim() === '') try { output.removeChild(d); } catch (_) {};
		if (d.textContent!.at(-1) === '\n') d.textContent = d.textContent!.slice(0, -1);
	};
	
	const newDiv = () => {
		d = div('quad', '');
		output.appendChild(d);
	};
	
	const createImage = (id: number | undefined, width: number, height: number) => {
		endDiv();
		const canvas = document.createElement('canvas');
		canvas.className = 'image';
		canvas.width = width;
		canvas.height = height;
		if (id !== undefined) {
			canvas.dataset.tinyaplId = id.toString();
			images[id] = canvas;
		}
		output.appendChild(canvas);
		newDiv();
		return canvas;
	};
	
	output.appendChild(clickableDiv('code', ' '.repeat(6) + code, code));
	newDiv();
	io.rInput(async what => { d.innerText += what + '\n'; });
	io.rOutput(async what => { d.innerText += what; });
	io.rError(async what => { d.innerText += what; });
	quads.rCreateImage(async (id, width, height) => { createImage(id, width, height); });
	quads.rDisplayImage(async (id, data) => {
		let canvas;
		if (id !== undefined)
			canvas = images[id];
		else
			canvas = createImage(id, data.width, data.height);
		const ctx = canvas.getContext('2d')!;
		ctx.clearRect(0, 0, canvas.width, canvas.height);
		ctx.putImageData(data, 0, 0);
	});
	quads.rScatterPlot(async (xs, ys, mode) => {
		endDiv();
		const traces = xs.map((x, i) => ({ x, y: ys[i], mode, type: 'scatter', line: { shape: 'spline' } }));
		const d = div('plot', '');
		output.appendChild(d);
		// @ts-expect-error Plotly global not typed
		Plotly.newPlot(d, traces, { showlegend: false }, { responsive: true });
		newDiv();
	});
	quads.rPlayAudio(async buf => {
		endDiv();
		const audio = document.createElement('audio');
  	audio.controls = true;
		audio.autoplay = true;
  	const blob = new Blob([buf], { type: 'audio/wav' });
  	const url = URL.createObjectURL(blob);
  	audio.src = url;
  	output.appendChild(audio);
		newDiv();
	});
	const [result, success] = await tinyapl.runCode(context, code);
	io.done();
	quads.dCreateImage();
	quads.dDisplayImage();
	quads.dPlayAudio();
	quads.dScatterPlot();
	endDiv();
	if (success) output.appendChild(clickableDiv('result', result));
	else output.appendChild(div('error', result));
}

async function run() {
	await runCode(input.value);
	input.value = '';
	highlight();
}

let keyboardState = 0;

button.addEventListener('click', () => run());
input.addEventListener('keydown', evt => {
	if (keyboardState < 2 && evt.code == prefix.code && !evt.shiftKey) {
		keyboardState++
		evt.preventDefault();
	} else if (keyboardState !== 0 && !evt.altKey && !evt.ctrlKey && !evt.metaKey) {
		const v = keyboard.find(k => k.code == evt.code);
		if (v) {
			const t = keyboardState == 2 ? (evt.shiftKey ? v.symPPS : v.symPP) : (evt.shiftKey ? v.symPS : v.symP);
			insertText(t ?? evt.key);
			keyboardState = 0;
			evt.preventDefault();
		}
	} else if (evt.key == 'Enter') {
		evt.preventDefault();
		return run();
	}
});
input.addEventListener('input', () => highlight());
input.addEventListener('scroll', () => highlight());

const search = new URLSearchParams(window.location.search);

for (const line of search.getAll('run')) await runCode(decodeURIComponent(line));

