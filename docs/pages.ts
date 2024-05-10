import { Page } from './types.d.ts';
import { readFrontmatter, renderMarkdown } from './markdown.ts';
import { recordGetter } from './utils.ts';

function readPage(path: string, src: string): Page {
	const frontmatter = recordGetter(readFrontmatter(src));
	const body = renderMarkdown(src);
	return {
		glyph: frontmatter.get('glyph') ?? '',
		pattern: frontmatter.get('pattern') ?? '',
		name: frontmatter.get('name') ?? path,
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