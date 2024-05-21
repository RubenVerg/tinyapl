import pages, { loadPages } from './pages.ts';

await loadPages();

await Deno.writeTextFile('pages.json', JSON.stringify(pages));