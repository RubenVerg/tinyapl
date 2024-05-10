/** @jsx h */

import DocsPage from './components/DocsPage.tsx';
import FullPage from './components/FullPage.tsx';
import Index from './components/Index.tsx';
import { Page } from './types.d.ts';
import pages from './pages.ts';

import { serveDir } from './deps/std/http.ts';
import html, { h, HtmlOptions } from './deps/x/htm.ts';

const stylesheets = [
	'https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/bootstrap.min.css',
	'https://cdn.jsdelivr.net/npm/bootstrap-icons@1.11.3/font/bootstrap-icons.min.css',
	'https://cdn.jsdelivr.net/npm/katex@0.16.0/dist/katex.min.css',
	'/style.css',
];

const scripts = [
	'https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/js/bootstrap.bundle.min.js',
];

const sharedOptions: Partial<HtmlOptions> = {
	lang: 'en',
	links: [
		...stylesheets.map(href => ({ href, rel: 'stylesheet' })),
		{ href: '/logo.svg', type: 'image/svg+xml', rel: 'icon' },
	],
	scripts: scripts.map(src => ({ src })),
	meta: {
		viewport: 'width=device-width, initial-scale=1',
	},
}

function render(page: JSX.Element, options: Partial<HtmlOptions>) {
	return html({
		...sharedOptions,
		body: page,
		...options,
	});
}

const index = await render(<FullPage pages={pages}><Index /></FullPage>, {
	title: 'TinyAPL',
});

const docPage = (page: Page) => render(<FullPage pages={pages}><DocsPage page={page} /></FullPage>, {
	title: `${page.name} - TinyAPL`,
});

async function handler(req: Request) {
	const { pathname } = new URL(req.url);
	
	if (pathname === '/') {
		return index;
	}
	if (pathname.replaceAll('/', '') in pages) {
		return await docPage(pages[pathname.replaceAll('/', '')]);
	}

	return await serveDir(req, { fsRoot: './assets' });
}

Deno.serve(handler);