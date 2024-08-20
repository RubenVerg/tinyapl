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
			<button class='btn btn-outline-secondary' data-bs-toggle='collapse' data-bs-target='#planned-primitives-collapse'>Planned primitives</button>
			<div class='collapse' id='planned-primitives-collapse'>
				<ul>{Object.entries(pages.primitives).filter(([_k, v]) => v.planned).toSorted(([_ka, va], [_kb, vb]) => va.name.localeCompare(vb.name)).map(([k, _v]) => <li><PrimitiveLink key={k} /></li>)}</ul>
			</div>
		</div>
		<p>Quad name count: {Object.values(pages.quads).length}</p>
		<p>Of which planned: {Object.values(pages.quads).filter(p => p.planned).length}</p>
		<p>Of which deprecated: {Object.values(pages.quads).filter(p => p.deprecated).length}</p>
		<div>
			<button class='btn btn-outline-secondary' data-bs-toggle='collapse' data-bs-target='#planned-quads-collapse'>Planned quads</button>
			<div class='collapse' id='planned-quads-collapse'>
				<ul>{Object.entries(pages.quads).filter(([_k, v]) => v.planned).toSorted(([_ka, va], [_kb, vb]) => va.name.localeCompare(vb.name)).map(([k, v]) => <li><a href={`/docs/quads/${k}`}><code>{v.glyph}</code> {v.name}</a></li>)}</ul>
			</div>
		</div>
		<p>Glyph count: {Object.values(pages.glyphs).length}</p>
	</>;
}

export default Data;
