/** @jsx h */
/** @jsxFrag Fragment */

import { Fragment, h } from '../deps/x/htm.ts';

function Index() {
	return <>
		<h1><img width='64' height='64' src='/logo.svg' /> TinyAPL</h1>
		<p>
			TinyAPL is a tiny APL implemented in Haskell.
		</p>
		<p>
			<a class='btn btn-primary me-1' href='https://github.com/RubenVerg/tinyapl/releases/'><i class='bi bi-download' /> Download</a>
			(requires <a href='https://wasmtime.dev/'>Wasmtime</a> or another WASM runtime)
		</p>
	</>;
}

export default Index;