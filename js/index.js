import * as tinyapl from './tinyapl.js';

const buttons = document.querySelector('#buttons');
const output = document.querySelector('#output');
/** @type {HTMLInputElement} */
const input = document.querySelector('#input');
const highlighted = document.querySelector('#highlighted');
const button = document.querySelector('#button');

function zip(as, bs) {
	return [...as, ...bs].slice(0, Math.min(as.length, bs.length)).map((_, idx) => [as[idx], bs[idx]]);
}

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
		span.style.color = colors[tinyapl.colorsInv[c]];
		span.innerText = t;
		highlighted.appendChild(span);
	}
	highlighted.scrollLeft = input.scrollLeft;
}

const io = new class IO {
	#input = [];
	#output = [];
	#error = [];
	rInput(l) { this.#input.push(l); }
	rOutput(l) { this.#output.push(l); }
	rError(l) { this.#error.push(l); }
	done() { this.#input = []; this.#output = []; this.#error = []; }
	async input() { const i = window.prompt('Input'); for (const l of this.#input) await l(i); return i; }
	async output(what) { for (const l of this.#output) await l(what); }
	async error(what) { for (const l of this.#error) await l(what); }
};

for (const k of ['syntax', 'identifiers', 'arrays', 'functions', 'adverbs', 'conjunctions']) {
	for (const i of tinyapl.glyphs[k]) {
		const b = document.createElement('button');
		b.textContent = i;
		b.addEventListener('click', () => {
			input.setRangeText(i, input.selectionStart, input.selectionEnd, "end");
			input.focus();
			highlight();
		});
		buttons.appendChild(b);
	}
	buttons.appendChild(document.createElement('br'));
}

const context = await tinyapl.newContext(io.input.bind(io), io.output.bind(io), io.error.bind(io));

function div(cls, contents) {
	const div = document.createElement('div');
	div.className = cls;
	div.textContent = contents;
	return div;
}

function clickableDiv(cls, contents, clickedContents = contents) {
	const d = div(cls, contents);
	d.addEventListener('click', () => {
		if (input.value.trim() == '') {
			input.value = clickedContents;
			input.focus();
		}
	});
	return d;
}

async function run() {
	const code = input.value;
	output.appendChild(clickableDiv('code', ' '.repeat(6) + code, code));
	const d = div('quad', '');
	output.appendChild(d);
	io.rInput(what => { d.innerText += what + '\n'; });
	io.rOutput(what => { d.innerText += what; });
	io.rError(what => { d.innerText += what; });
	const [result, success] = await tinyapl.runCode(context, code);
	io.done();
	if (d.textContent.trim() === '') output.removeChild(d);
	if (d.textContent.at(-1) === '\n') d.textContent = d.textContent.slice(0, -1);
	if (success) output.appendChild(clickableDiv('result', result));
	else output.appendChild(div('error', result));
	input.value = '';
	highlight();
}

button.addEventListener('click', () => run());
input.addEventListener('keydown', evt => {
	if (evt.key == 'Enter') {
		evt.preventDefault();
		return run();
	}
});
input.addEventListener('input', () => highlight());
input.addEventListener('scroll', () => highlight());
