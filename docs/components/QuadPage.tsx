/** @jsx h */
/** @jsxFrag Fragment */

import { DeprecatedAlert, DeprecatedBadge, PlannedAlert, PlannedBadge } from './Banners.tsx';
import { Quad } from '../types.d.ts';

import { Fragment, h } from '../deps/x/htm.ts';

export interface QuadPageProps {
	quad: Quad;
}

function QuadPage({ quad }: QuadPageProps) {
	return <>
		<h1>{quad.planned && <PlannedBadge />}{quad.deprecated && <DeprecatedBadge />}<code>{quad.glyph}</code> {quad.name} <code class='float-end'>{quad.pattern}</code></h1>

		{quad.planned && <PlannedAlert />}

		{quad.deprecated && <DeprecatedAlert />}

		<div><em>Category: {quad.category}</em></div>

		<div dangerouslySetInnerHTML={{ __html: quad.body }} />
	</>;
}

export default QuadPage;