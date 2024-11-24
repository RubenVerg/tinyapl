/** @jsx h */
/** @jsxFrag Fragment */

import Header from './Header.tsx';
import { Pages } from '../types.d.ts';

import { Fragment, h } from '../deps/x/htm.ts';

export interface FullPageProps {
	pages: Pages;
	children?: string | JSX.Element | JSX.Element[];
}

function FullPage({ pages, children }: FullPageProps) {
	return <>
		<Header pages={pages} />
		<main class='container'>{children}</main>
	</>;
}

export default FullPage;