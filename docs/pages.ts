import { Page } from './types.d.ts';
import { readFrontmatter, renderMarkdown } from './markdown.ts';
import { recordGetter } from './utils.ts';

function readPage(path: string, src: string): Page {
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

const pages: Record<string, Page> = {};

for await (const file of Deno.readDir('pages')) {
	if (file.isFile && file.name.endsWith('.md')) {
		const path = file.name.slice(0, -3);
		pages[path] = readPage(path, await Deno.readTextFile(`pages/${file.name}`));
	}
}

export default pages;