/** @jsx h */
/** @jsxFrag Fragment */

import { Fragment, h } from '../deps/x/htm.ts';

export interface IndexProps {
	body: string;
}

function Index({ body }: IndexProps) {
	return <div dangerouslySetInnerHTML={{ __html: body }} />;
}

export default Index;