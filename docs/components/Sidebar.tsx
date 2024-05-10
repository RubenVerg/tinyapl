/** @jsx h */

import { Page } from '../types.d.ts';

import { h } from '../deps/x/htm.ts';

export interface SidebarProps {
	id: string;
	pages: Record<string, Page>;
}

function Sidebar({ id, pages }: SidebarProps) {
	return <div id={id} class='offcanvas offcanvas-start w-25' tabindex={-1} data-bs-keyboard='false' data-bs-backdrop='false'>
		<div class='offcanvas-header'>
			<h6 class='offcanvas-title d-none d-sm-block'>Docs</h6>
			<button type='button' class='btn-close text-reset' data-bs-dismiss={id} />
		</div>
		<div class='offcanvas-body'>
			<ul class='nav flex-column mb-0 mb-sm-auto align-items-start'>
				<li class='nav-item'>
					<a class='nav-link' href='/'>Index</a>
				</li>
				{[...Object.entries(pages)].sort(([ak, _av], [bk, _bv]) => ak.localeCompare(bk)).map(([id, page]) => <li class='nav-item'>
					<a class='nav-link' href={`/${id}`}><code>{page.glyph}</code> {page.name}</a>
				</li>)}
			</ul>
		</div>
	</div>;
}

export default Sidebar;