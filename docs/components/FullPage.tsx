/** @jsx h */
/** @jsxFrag Fragment */

import Sidebar from './Sidebar.tsx';
import { Page } from '../types.d.ts';

import { Fragment, h } from '../deps/x/htm.ts';

export interface FullPageProps {
	pages: Record<string, Page>;
	children?: string | JSX.Element | JSX.Element[];
}

function FullPage({ pages, children }: FullPageProps) {
	return <>
		<Sidebar id='sidebar' pages={pages} />
		<button type='button' class='btn' data-bs-toggle='offcanvas' data-bs-target='#sidebar'><i class='bi bi-list' /></button>
		<main class='container'>{children}</main>
	</>;
}

export default FullPage;