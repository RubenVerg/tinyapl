import { newScope, runCode } from './tinyapl.js';

const output = document.querySelector('#output');
const input = document.querySelector('#input');
const button = document.querySelector('#button');

const scope = await newScope();

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
	const [result, success] = await runCode(scope, code);
	if (success) output.appendChild(clickableDiv('result', result));
	else output.appendChild(div('error', result));
	input.value = '';
}

button.addEventListener('click', () => run());
input.addEventListener('keyup', evt => {
	if (evt.key == 'Enter') return run();
});
