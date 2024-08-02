import { newContext, runCode, glyphs } from './tinyapl.js';

const buttons = document.querySelector('#buttons');
const output = document.querySelector('#output');
/** @type {HTMLInputElement} */
const input = document.querySelector('#input');
const button = document.querySelector('#button');

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
	for (const i of glyphs[k]) {
		const b = document.createElement('button');
		b.textContent = i;
		b.addEventListener('click', () => {
			input.setRangeText(i, input.selectionStart, input.selectionEnd, "end");
			input.focus();
		});
		buttons.appendChild(b);
	}
	buttons.appendChild(document.createElement('br'));
}

const context = await newContext(io.input.bind(io), io.output.bind(io), io.error.bind(io));

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
	const [result, success] = await runCode(context, code);
	io.done();
	if (d.textContent.trim() === '') output.removeChild(d);
	if (d.textContent.at(-1) === '\n') d.textContent = d.textContent.slice(0, -1);
	if (success) output.appendChild(clickableDiv('result', result));
	else output.appendChild(div('error', result));
	input.value = '';
}

button.addEventListener('click', () => run());
input.addEventListener('keyup', evt => {
	if (evt.key == 'Enter') return run();
});

