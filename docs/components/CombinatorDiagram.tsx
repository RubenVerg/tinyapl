/** @jsx h */

import pages from '../pages.ts';

import { h } from '../deps/x/htm.ts';

export interface CombinatorDiagramProps {
	key: string;
	name: string;
}

function CombinatorDiagram({ key, name }: CombinatorDiagramProps) {
	return <img src={`/combinators/${key}.svg`} width={128} alt={`${name} diagram`} />;
}

export default CombinatorDiagram;
