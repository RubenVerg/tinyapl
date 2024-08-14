/** @jsx h */

import pages from '../pages.ts';

import { h } from '../deps/x/htm.ts';

export interface PrimitiveLinkProps {
	key: string;
	short?: boolean;
	children?: string | JSX.Element | JSX.Element[];
}

function PrimitiveLink({ key, short, children }: PrimitiveLinkProps) {
	const primitive = pages.primitives[key];
	if (primitive === undefined) throw new Error(`Primitive ${key} not found`);
	return <a href={`/docs/primitive/${key}`} class='link-underline link-underline-opacity-0 link-underline-opacity-75-hover'><code>{primitive.glyph}</code>{short ? '' : children ? [' ', children] : [' ', primitive.name]}</a>;
}

export default PrimitiveLink;
