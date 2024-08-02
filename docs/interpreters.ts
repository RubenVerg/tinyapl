import { Interpreters } from './types.d.ts';

let interpreters: Interpreters = [];

export async function loadInterpreters(): Promise<void> {
	interpreters = [];
	
	for await (const file of Deno.readDir('interpreters')) {
		if (file.isDirectory)
			interpreters.push(file.name);
	}
}

try {
	interpreters = JSON.parse(await Deno.readTextFile('interpreters.json'));
} catch {
	await loadInterpreters();
}

export default interpreters;
