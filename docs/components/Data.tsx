/** @jsx h */
/** @jsxFrag Fragment */

import { h, Fragment } from '../deps/x/htm.ts';
import { Pages } from '../types.d.ts';
import PrimitiveLink from "./PrimitiveLink.tsx";

export interface DataProps {
	pages: Pages;
}

function Data({ pages }: DataProps) {
	return <>
		<h1>Data</h1>
		<p>Primitive count: {Object.values(pages.primitives).length}</p>
		<p>Of which planned: {Object.values(pages.primitives).filter(p => p.planned).length}</p>
		<p>Of which deprecated: {Object.values(pages.primitives).filter(p => p.deprecated).length}</p>
		<div>
			<button class='btn btn-outline-secondary' data-bs-toggle='collapse' data-bs-target='#planned-collapse'>Planned primitives</button>
			<div class='collapse' id='planned-collapse'>
				<ul>{Object.entries(pages.primitives).filter(([_k, v]) => v.planned).toSorted(([_ka, va], [_kb, vb]) => va.name.localeCompare(vb.name)).map(([k, _v]) => <li><PrimitiveLink key={k} /></li>)}</ul>
			</div>
		</div>
		<p>Glyph count: {Object.values(pages.glyphs).length}</p>
	</>;
}

export default Data;
