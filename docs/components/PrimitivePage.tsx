/** @jsx h */
/** @jsxFrag Fragment */

import { PlannedAlert, PlannedBadge } from './Planned.tsx';
import { Primitive } from '../types.d.ts';

import { Fragment, h } from '../deps/x/htm.ts';

export interface PrimitivePageProps {
	primitive: Primitive;
}

function PrimitivePage({ primitive }: PrimitivePageProps) {
	return <>
		<h1>{primitive.planned && <PlannedAlert />}<code>{primitive.glyph}</code> {primitive.name} <code class='float-end'>{primitive.pattern}</code></h1>

		{primitive.planned && <PlannedBadge />}

		<div dangerouslySetInnerHTML={{ __html: primitive.body }} />
	</>;
}

export default PrimitivePage;