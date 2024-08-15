/** @jsx h */
/** @jsxFrag Fragment */

import { Glyph } from '../types.d.ts';

import { h, Fragment } from '../deps/x/htm.ts';
import pages from "../pages.ts";

export interface GlyphPageProps {
	glyph: Glyph;
}

export default function GlyphPage({ glyph }: GlyphPageProps) {
	return <>
		<h1><code>{glyph.glyph}</code> {glyph.name}</h1>

		<ul>
			{glyph.primitives.map(p => <li><a href={`/docs/primitive/${p}`} class='link-underline link-underline-opacity-0 link-underline-opacity-75-hover'>{pages.primitives[p].name}</a></li>)}
		</ul>

		{glyph.body}
	</>;
}