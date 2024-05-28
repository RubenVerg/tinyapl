/** @jsx h */

import { Pages } from '../types.d.ts';

import { h } from '../deps/x/htm.ts';

export interface SidebarProps {
	id: string;
	pages: Pages;
}

function Sidebar({ id, pages }: SidebarProps) {
	const navClass = 'nav flex-column mb-0 mb-sm-auto align-items-start', subNavClass = `${navClass} ms-1`;

	return <div id={id} class='offcanvas offcanvas-start w-auto' tabindex={-1} data-bs-keyboard='false' data-bs-backdrop='false'>
		<div class='offcanvas-header'>
			<h6 class='offcanvas-title d-none d-sm-block'>Docs</h6>
			<button type='button' class='btn-close text-reset' data-bs-dismiss='offcanvas' aria-label='Close' />
		</div>
		<div class='offcanvas-body'>
			<ul class={navClass}>
				<li class='nav-item'>
					<a class='nav-link' href='/'>Index</a>
				</li>
				<li class='nav-item ms-2'>
					Info
					<ul class={subNavClass}>
						{[...Object.entries(pages.info)].sort(([_ak, av], [_bk, bv]) => av.name.localeCompare(bv.name)).map(([id, info]) => <li class='nav-item'><a class='nav-link' href={`/info/${id}`}>{info.name}</a></li>)}
					</ul>
				</li>
				<li class='nav-item ms-2'>
					Primitives
					<ul class={subNavClass}>
						{[...Object.entries(pages.primitives)].sort(([_ak, av], [_bk, bv]) => av.name.localeCompare(bv.name)).map(([id, primitive]) => <li class='nav-item'><a class='nav-link' href={`/primitive/${id}`}><code>{primitive.glyph}</code> {primitive.name}</a></li>)}
					</ul>
				</li>
				<li class='nav-item ms-2'>
					Quad Names
					<ul class={subNavClass}>
						{[...Object.entries(pages.quads)].sort(([_ak, av], [_bk, bv]) => av.glyph.localeCompare(bv.glyph)).map(([id, quad]) => <li class='nav-item'><a class='nav-link' href={`/quad/${id}`}><code>{quad.glyph}</code> {quad.name}</a></li>)}
					</ul>
				</li>
			</ul>
		</div>
	</div>;
}

export default Sidebar;