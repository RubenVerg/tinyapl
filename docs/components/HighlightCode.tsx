/** @jsx h */

import { h } from '../deps/x/htm.ts';

import * as tinyapl from '../interpreters/latest/tinyapl.js';

const colors = {
	other: 'text-body-secondary',
	syntax: 'text-body-emphasis',
	number: 'text-danger-emphasis',
	string: 'text-primary',
	stringEscaoe: 'text-primary-emphasis',
	arrayName: 'text-danger',
	primArray: 'text-danger-emphasis',
	functionName: 'text-success',
	primFunction: 'text-success-emphasis',
	adverbName: 'text-info',
	primAdverb: 'text-info-emphasis',
	conjunctionName: 'text-warning',
	primConjunction: 'text-warning-emphasis',
	comment: 'text-secondary',
}

function zip<A, B>(as: A[], bs: B[]): [A, B][] {
	return [...as, ...bs].slice(0, Math.min(as.length, bs.length)).map((_, idx) => [as[idx], bs[idx]]);
}

export interface HighlightCodeProps {
	code: string;
}

async function HighlightCode({ code: c }: HighlightCodeProps) {
	console.log('<hl>', c);
	const code = <code></code>;
	console.log('highlighting', c);
	const chars = await tinyapl.splitString(c);
	const hl = await tinyapl.highlight(c);
	console.log('highlghted', hl);
	for (const [t, c] of zip(chars, hl)) code.children.push(
		<span class={`hl-char hl-char-${tinyapl.colorsInv[c]} ${colors[tinyapl.colorsInv[c] as keyof typeof colors]}`}>{t}</span>
	);
	console.log('now children are', code.children);
	return code;
}

export default HighlightCode;
