/** @jsx h */
/** @jsxFrag Fragment */

import { Pages } from '../types.d.ts';

import { Fragment, h } from '../deps/x/htm.ts';

export interface PrimitivesProps {
	pages: Pages;
}

function Primitives({ pages }: PrimitivesProps) {
	return <>
		<h1>Primitives</h1>
		
		<ul>
			{[...Object.entries(pages.primitives)].sort(([_ak, av], [_bk, bv]) => av.name.localeCompare(bv.name)).map(([id, primitive]) => <li><a href={`/docs/primitive/${id}`}><code>{primitive.glyph}</code> {primitive.name}</a></li>)}
		</ul>
	</>;
}

export default Primitives;