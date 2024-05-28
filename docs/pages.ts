import { Info, Primitive, Quad, Pages } from './types.d.ts';
import { readFrontmatter, renderMarkdown } from './markdown.ts';
import { recordGetter } from './utils.ts';

function readInfo(path: string, src: string): Info {
	const frontmatter = recordGetter(readFrontmatter(src));
	const body = renderMarkdown(src);
	const name = frontmatter.get<string>('name') ?? (console.warn(`No name for ${path}`), path);
	return {
		name,
		planned: frontmatter.get('planned') ?? false,
		body,
	};
}

function readPrimitive(path: string, src: string): Primitive {
	const frontmatter = recordGetter(readFrontmatter(src));
	const body = renderMarkdown(src);
	const name = frontmatter.get<string>('name') ?? (console.warn(`No name for ${path}`), path);
	const glyph = frontmatter.get<string>('glyph') ?? (console.warn(`No glyph for ${name}`), '');
	const pattern = frontmatter.get<string>('pattern') ?? (console.warn(`No pattern for ${name}`), '');
	return {
		glyph,
		pattern,
		name,
		planned: frontmatter.get('planned') ?? false,
		deprecated: frontmatter.get('deprecated') ?? false,
		body,
	};
}

function readQuad(path: string, src: string): Quad {
	const frontmatter = recordGetter(readFrontmatter(src));
	const body = renderMarkdown(src);
	const name = frontmatter.get<string>('name') ?? (console.warn(`No name for ${path}`), path);
	const glyph = frontmatter.get<string>('glyph') ?? (console.warn(`No glyph for ${name}`), '');
	const pattern = frontmatter.get<string>('pattern') ?? (console.warn(`No pattern for ${name}`), '');
	const category = frontmatter.get<string>('category') ?? (console.warn(`No category for ${name}`), 'Core');
	return {
		glyph,
		pattern,
		name,
		category,
		planned: frontmatter.get('planned') ?? false,
		deprecated: frontmatter.get('deprecated') ?? false,
		body,
	};
}

let pages: Pages = { index: '', info: {}, primitives: {}, quads: {} };

export async function loadPages(): Promise<void> {
	pages.index = renderMarkdown(await Deno.readTextFile('pages/index.md'));

	for await (const file of Deno.readDir('pages/info')) {
		if (file.isFile && file.name.endsWith('.md')) {
			const path = file.name.slice(0, -3);
			pages.info[path] = readInfo(path, await Deno.readTextFile(`pages/info/${file.name}`));
		}
	}

	for await (const file of Deno.readDir('pages/primitives')) {
		if (file.isFile && file.name.endsWith('.md')) {
			const path = file.name.slice(0, -3);
			pages.primitives[path] = readPrimitive(path, await Deno.readTextFile(`pages/primitives/${file.name}`));
		}
	}

	for await (const file of Deno.readDir('pages/quads')) {
		if (file.isFile && file.name.endsWith('.md')) {
			const path = file.name.slice(0, -3);
			pages.quads[path] = readQuad(path, await Deno.readTextFile(`pages/quads/${file.name}`))
		}
	}
}


try {
	pages = JSON.parse(await Deno.readTextFile('pages.json'));
} catch {
	await loadPages();
}

export default pages;