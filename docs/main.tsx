/** @jsx h */

import InfoPage from './components/InfoPage.tsx';
import PrimitivePage from './components/PrimitivePage.tsx';
import FullPage from './components/FullPage.tsx';
import Index from './components/Index.tsx';
import { Info, Primitive } from './types.d.ts';
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

const infoPage = (info: Info) => render(<FullPage pages={pages}><InfoPage info={info} /></FullPage>, {
	title: `${info.name} - TinyAPL`,
});

const infoPages: Record<string, Response> = Object.fromEntries(await Promise.all([...Object.entries(pages.info)].map(async ([k, v]) => [k, await infoPage(v)])));

const primitivePage = (primitive: Primitive) => render(<FullPage pages={pages}><PrimitivePage primitive={primitive} /></FullPage>, {
	title: `${primitive.name} - TinyAPL`,
});

const primitivePages: Record<string, Response> = Object.fromEntries(await Promise.all([...Object.entries(pages.primitives)].map(async ([k, v]) => [k, await primitivePage(v)])));

const allPages = {
	info: infoPages,
	primitive: primitivePages,
};

async function handler(req: Request) {
	const { pathname } = new URL(req.url);
	
	if (pathname === '/') {
		return index;
	}

	for (const t of Object.keys(allPages)) {
		if (pathname.startsWith(`/${t}`) && pathname.replace(`/${t}`, '').replaceAll('/', '') in allPages[t as keyof typeof allPages]) {
			return allPages[t as keyof typeof allPages][pathname.replace(`/${t}`, '').replaceAll('/', '')];
		}
	}
	
	return await serveDir(req, { fsRoot: './assets' });
}

Deno.serve(handler);