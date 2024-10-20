/** @jsx h */

import GlyphPage from './components/GlyphPage.tsx';
import InfoPage from './components/InfoPage.tsx';
import PrimitivePage from './components/PrimitivePage.tsx';
import QuadPage from './components/QuadPage.tsx';
import FullPage from './components/FullPage.tsx';
import Index from './components/Index.tsx';
import Data from './components/Data.tsx';
import RunInterpreter from './components/RunInterpreter.tsx';
import { Info, Primitive, Quad, Pages, Glyph } from './types.d.ts';
import pages, { forcePages, loadPages, validatePages } from './pages.ts';
import interpreters from './interpreters.ts';

import { serveDir } from './deps/std/http.ts';
import html, { h, HtmlOptions } from './deps/x/htm.ts';
import { extensions, types } from './deps/std/media_types/_db.ts';

types.set('tinyapl', 'text/plain');
extensions.set('text/plain', [...extensions.get('text/plain')!, 'tinyapl']);

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

validatePages();
await forcePages();

async function render(page: JSX.Element, options: Partial<HtmlOptions>) {
	return await html({
		...sharedOptions,
		body: page,
		...options,
	});
}

const index = () => render(<FullPage pages={pages}><Index body={pages.index} /></FullPage>, {
	title: 'TinyAPL',
});

const infoPage = (info: Info) => render(<FullPage pages={pages}><InfoPage info={info} /></FullPage>, {
	title: `${info.name} - TinyAPL`,
});

const primitivePage = (primitive: Primitive) => render(<FullPage pages={pages}><PrimitivePage primitive={primitive} /></FullPage>, {
	title: `${primitive.name} - TinyAPL`,
});

const quadPage = (quad: Quad) => render(<FullPage pages={pages}><QuadPage quad={quad} /></FullPage>, {
	title: `${quad.name} - TinyAPL`,
});

const glyphPage = (glyph: Glyph) => render(<FullPage pages={pages}><GlyphPage glyph={glyph} /></FullPage>, {
	title: `${glyph.glyph} ${glyph.name} - TinyAPL`,
});

const runInterpreter = await render(<RunInterpreter pages={pages} interpreters={interpreters} />, {
	title: 'Run Interpreter - TinyAPL',
});

const data = await render(<FullPage pages={pages}><Data pages={pages} /></FullPage>, {
	title: 'Data - TinyAPL',
});

const directories: Record<string, keyof typeof pages> = {
	glyph: 'glyphs',
	info: 'info',
	primitive: 'primitives',
	quad: 'quads',
}

const renderers = {
	index: index,
	glyphs: glyphPage,
	info: infoPage,
	primitives: primitivePage,
	quads: quadPage,
};

if (Deno.args.includes('--dev')) {
	// In dev mode, render all pages once to be sure that they don't have mistakes.
	console.log('Checking pages...');
	await loadPages();
	for (const [typ, ps] of Object.entries(pages)) {
		if (typ === 'index') continue;
		for (const p of Object.keys(ps)) {
			await (renderers[typ as keyof typeof pages] as (p: unknown) => Promise<Response>)(ps[p]).then(x => x.text());
		}
	}
}

async function handler(req: Request) {
	const { pathname } = new URL(req.url);
	
	if (pathname === '/') {
		return index();
	}

	if (pathname === '/data' || pathname === '/data/') {
		return data;
	}

	if (pathname === '/run' || pathname === '/run/') {
		return runInterpreter;
	}

	if (pathname.startsWith('/run')) {
		const v = pathname.split('/')[2];
		if (interpreters.includes(v)) {
			return serveDir(req, {
				urlRoot: `run/${v}`,
				fsRoot: `interpreters/${v}`,
			});
		}
	}

	for (const [dir, typ] of Object.entries(directories)) {
		if (typ === 'index') continue;
		if (pathname.startsWith(`/${dir}`)) {
			const u = new URL(req.url);
			u.pathname = `/docs${pathname}`;
			return Response.redirect(u);
		}
		if (pathname.startsWith(`/docs/${dir}`)) {
			const path = pathname.replace(`/docs/${dir}`, '').replaceAll('/', '');
			return (renderers[typ] as (p: unknown) => Promise<Response>)((pages as Pick<Pages, Exclude<keyof Pages, 'index'>>)[typ][path]);
		}
	}
	
	return await serveDir(req, { fsRoot: './assets' });
}

Deno.serve(handler);