import { newContext, runCode, glyphs } from './tinyapl.js';

const buttons = document.querySelector('#buttons');
const output = document.querySelector('#output');
/** @type {HTMLInputElement} */
const input = document.querySelector('#input');
const button = document.querySelector('#button');

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

const context = await newContext();

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
	const [result, success] = await runCode(context, code);
	if (success) output.appendChild(clickableDiv('result', result));
	else output.appendChild(div('error', result));
	input.value = '';
}

button.addEventListener('click', () => run());
input.addEventListener('keyup', evt => {
	if (evt.key == 'Enter') return run();
});

