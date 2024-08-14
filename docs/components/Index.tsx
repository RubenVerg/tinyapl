/** @jsx h */
/** @jsxFrag Fragment */

import { Fragment, h } from '../deps/x/htm.ts';

export interface IndexProps {
	body: JSX.Element;
}

function Index({ body }: IndexProps) {
	return body;
}

export default Index;