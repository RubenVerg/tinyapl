/** @jsx h */
/** @jsxFrag Fragment */

import { Interpreters, Pages } from '../types.d.ts';
import FullPage from './FullPage.tsx';

import { Fragment, h } from '../deps/x/htm.ts';
import { compare, parse } from '../deps/std/semver.ts';

export interface RunInterpreterProps {
	pages: Pages;
	interpreters: Interpreters;
}

export default function RunInterpreter({ pages, interpreters }: RunInterpreterProps) {
	return <FullPage pages={pages}>
		<h1>Run Interpreter</h1>
		<p>Select version</p>
		<ul>
		{interpreters
			.toSorted((a, b) => a === 'latest' ? -1 : b === 'latest' ? 1 : -compare(parse(a), parse(b)))
			.map(v => <li><a href={`/run/${v}`}>{v}</a></li>)}
		</ul>
	</FullPage>;
}
