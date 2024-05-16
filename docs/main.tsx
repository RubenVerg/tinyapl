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

const primitivePage = (primitive: Primitive) => render(<FullPage pages={pages}><PrimitivePage primitive={primitive} /></FullPage>, {
	title: `${primitive.name} - TinyAPL`,
});

const directories: Record<string, keyof typeof pages> = {
	info: 'info',
	primitive: 'primitives',
}

const renderers = {
	info: infoPage,
	primitives: primitivePage,
};

if (Deno.args.includes('--dev')) {
	// In dev mode, render all pages once to be sure that they don't have mistakes.
	console.log('Checking pages...');
	for (const [typ, ps] of Object.entries(pages)) {
		for (const p of Object.keys(ps)) {
			await (renderers[typ as keyof typeof pages] as (p: unknown) => Promise<Response>)(ps[p]).then(x => x.text());
		}
	}
}

async function handler(req: Request) {
	const { pathname } = new URL(req.url);
	
	if (pathname === '/') {
		return index;
	}

	for (const [dir, typ] of Object.entries(directories)) {
		if (pathname.startsWith(`/${dir}`)) {
			const path = pathname.replace(`/${dir}`, '').replaceAll('/', '');
			return (renderers[typ] as (p: unknown) => Promise<Response>)(pages[typ][path]);
		}
	}
	
	return await serveDir(req, { fsRoot: './assets' });
}

Deno.serve(handler);