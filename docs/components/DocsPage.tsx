/** @jsx h */
/** @jsxFrag Fragment */

import { Page } from '../types.d.ts';

import { Fragment, h } from '../deps/x/htm.ts';

export interface DocsPageProps {
	page: Page;
}

function DocsPage({ page }: DocsPageProps) {
	return <>
		<h1>{page.planned && <span class='badge text-warning border border-warning bg-warning-subtle me-3'>Planned</span>}<code>{page.glyph}</code> {page.name} <code class='float-end'>{page.pattern}</code></h1>

		{page.planned && <div class='alert alert-warning'>
			This primitive or feature is <strong>planned</strong>, that is, it doesn't appear in the most recent released implementation of TinyAPL. Details may change before it is added!
		</div>}

		<div dangerouslySetInnerHTML={{ __html: page.body }} />
	</>;
}

export default DocsPage;