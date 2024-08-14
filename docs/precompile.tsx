import pages, { loadPages } from './pages.ts';
import interpreters, { loadInterpreters } from './interpreters.ts';
import { JSXNode } from './deps/x/htm.ts';

await loadPages();
await loadInterpreters();

await Deno.writeTextFile('pages.json', JSON.stringify({
	index: { body: pages.index.toString() },
	...Object.fromEntries(Object.entries(pages).filter(([k]) => k !== 'index').map(([k, v]: [string, Record<string, { body: JSX.Element }>]) => { return [ k, Object.fromEntries(Object.entries(v).map(([k1, v1]) => [ k1, { ...v1, body: v1.body.toString() } ])) ]; })),
}));
await Deno.writeTextFile('interpreters.json', JSON.stringify(interpreters));