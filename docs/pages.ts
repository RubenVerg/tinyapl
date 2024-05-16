import { Info, Primitive, Pages } from './types.d.ts';
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
		body,
	};
}

const pages: Pages = { info: {}, primitives: {} };

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

export default pages;