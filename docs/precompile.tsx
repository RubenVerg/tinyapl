import pages, { loadPages } from './pages.ts';
import interpreters, { loadInterpreters } from './interpreters.ts';

await loadPages();
await loadInterpreters();

await Deno.writeTextFile('pages.json', JSON.stringify(pages));
await Deno.writeTextFile('interpreters.json', JSON.stringify(interpreters));