/** @jsx h */
/** @jsxFrag Fragment */

import { DeprecatedAlert, DeprecatedBadge, PlannedAlert, PlannedBadge } from './Banners.tsx';
import { Primitive } from '../types.d.ts';

import { Fragment, h } from '../deps/x/htm.ts';

export interface PrimitivePageProps {
	primitive: Primitive;
}

function PrimitivePage({ primitive }: PrimitivePageProps) {
	return <>
		<h1>{primitive.planned && <PlannedBadge />}{primitive.deprecated && <DeprecatedBadge />}<code>{primitive.glyph}</code> {primitive.name} <code class='float-end'>{primitive.pattern}</code></h1>

		{primitive.planned && <PlannedAlert />}

		{primitive.deprecated && <DeprecatedAlert />}

		{primitive.body}
	</>;
}

export default PrimitivePage;