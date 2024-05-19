/** @jsx h */
/** @jsxFrag Fragment */

import { PlannedAlert, PlannedBadge } from './Planned.tsx';
import { Quad } from '../types.d.ts';

import { Fragment, h } from '../deps/x/htm.ts';

export interface QuadPageProps {
	quad: Quad;
}

function QuadPage({ quad }: QuadPageProps) {
	return <>
		<h1>{quad.planned && <PlannedBadge />}<code>{quad.glyph}</code> {quad.name} <code class='float-end'>{quad.pattern}</code></h1>

		{quad.planned && <PlannedAlert />}

		<em>Category: {quad.category}</em>

		<div dangerouslySetInnerHTML={{ __html: quad.body }} />
	</>;
}

export default QuadPage;