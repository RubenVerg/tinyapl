import { h, Fragment } from './deps/x/htm.ts';
import * as Hast from './deps/npm/@types/hast.ts';
import * as Mdast from './deps/npm/@types/mdast.ts';
import * as Unist from './deps/npm/@types/unist.ts';
import { Handlers as MdastHandlers, revert } from './deps/npm/mdast-util-to-hast.ts';
import { normalizeUri } from './deps/npm/micromark-util-sanitize-uri.ts';
import { pointStart, pointEnd } from './deps/npm/unist-util-position.ts';
import rehypeFormat from './deps/npm/rehype-format.ts';
import rehypeKatex from './deps/npm/rehype-katex.ts';
import remarkFrontmatter from './deps/npm/remark-frontmatter.ts';
import remarkGfm from './deps/npm/remark-gfm.ts';
import remarkMath from './deps/npm/remark-math.ts';
import remarkParse from './deps/npm/remark-parse.ts';
import remarkRehype from './deps/npm/remark-rehype.ts';
import { CompilerFunction, Plugin, Processor, unified } from './deps/npm/unified.ts';
import { visit as unistVisit } from './deps/npm/unist-util-visit.ts';
import { parse as parseYaml} from './deps/npm/yaml.ts';
import { mdxFromMarkdown, mdxToMarkdown } from './deps/npm/mdast-util-mdx.ts';
import { mdxjs } from './deps/npm/micromark-extension-mdxjs.ts';
import { toJsxRuntime, Options as ToJsxRuntimeOptions, Jsx, Components } from './deps/npm/hast-util-to-jsx-runtime.ts';
import * as acorn from './deps/npm/acorn.ts';
import Expression from './deps/npm/eval-estree-expression.ts';

import { DeprecatedAlert, PlannedAlert } from './components/Banners.tsx';
import CombinatorDiagram from './components/CombinatorDiagram.tsx';
import PrimitiveLink from './components/PrimitiveLink.tsx';

const handlers: MdastHandlers = {
	heading(state, node) {
		const heading = node as Mdast.Heading;
		const result = {
			type: 'element',
			tagName: `h${heading.depth}`,
			properties: {
				class: 'mt-3'
			},
			children: state.all(node),
		} as const;
		state.patch(node, result);
		return state.applyData(node, result);
	},
	link(state, node) {
		const link = node as Mdast.Link;
		const properties = {
			href: normalizeUri(link.url),
			...link.title && { title: link.title },
			class: 'link-underline link-underline-opacity-0 link-underline-opacity-75-hover',
		};
		const result = {
			type: 'element',
			tagName: 'a',
			properties,
			children: state.all(node),
		} as const;
		state.patch(node, result);
		return state.applyData(node, result);
	},
	linkReference(state, node) {
		const linkReference = node as Mdast.LinkReference;
		const def = state.definition(linkReference.identifier);
		if (!def) return revert(state, node);
		const properties = {
			href: normalizeUri(def.url ?? ''),
			...def.title && { title: def.title },
			class: 'link-body-emphasis link-underline-opacity-50',
		};
		const result = {
			type: 'element',
			tagName: 'a',
			properties,
			children: state.all(node),
		} as const;
		state.patch(node, result);
		return state.applyData(node, result);
	},
	footnoteReference(state, node) {
		const footnoteReference = node as Mdast.FootnoteReference;
		const id = String(footnoteReference.identifier).toUpperCase();
		const safeId = normalizeUri(id.toLowerCase());
		const index = state.footnoteOrder.indexOf(id);
		let counter: number;
		if (index === -1) {
			state.footnoteOrder.push(id);
			state.footnoteCounts[id] = 1;
			counter = state.footnoteOrder.length;
		} else {
			state.footnoteCounts[id]++;
			counter = index + 1;
		}

		const reuseCounter = state.footnoteCounts[id];

		const link = {
			type: 'element',
			tagName: 'a',
			properties: {
				href: '#' + state.clobberPrefix + 'fn-' + safeId,
				id:
					state.clobberPrefix +
					'fnref-' +
					safeId +
					(reuseCounter > 1 ? '-' + reuseCounter : ''),
				dataFootnoteRef: true,
				ariaDescribedBy: 'footnote-label',
				class: 'link-body-emphasis link-underline-opacity-50',
			},
			children: [{ type: 'text', value: `(${counter})` }] as Hast.ElementContent[],
		} as const;
		state.patch(node, link);

		const sup = {
			type: 'element',
			tagName: 'sup',
			properties: {},
			children: [link] as Hast.ElementContent[],
		} as const;
		state.patch(node, sup)
		return state.applyData(node, sup)
	},
	blockquote(state, node) {
		const result = {
			type: 'element',
			tagName: 'blockquote',
			properties: {
				class: 'border-start border-2 border-body-secondary ps-2'
			},
			children: state.wrap(state.all(node), true),
		} as const;
		state.patch(node, result);
		return state.applyData(node, result);
	},
	table(state, node) {
		const table = node as Mdast.Table;
		const rows = state.all(node);
		const firstRow = rows.shift();
		const tableContent: Hast.Element[] = [];
		if (firstRow) {
			const head: Hast.Content = {
				type: 'element',
				tagName: 'thead',
				properties: {},
				children: state.wrap([firstRow], true),
			};
			state.patch(node.children[0], head);
			tableContent.push(head)
		}

		if (rows.length > 0) {
			const body: Hast.Element & { position?: Unist.Position } = {
				type: 'element',
				tagName: 'tbody',
				properties: {},
				children: state.wrap(rows, true),
			}

			const start = pointStart(table.children[1]);
			const end = pointEnd(table.children.at(-1));
			if (start && end) body.position = { start, end };
			tableContent.push(body);
		}

		const result: Hast.Content = {
			type: 'element',
			tagName: 'table',
			properties: { class: 'table' },
			children: state.wrap(tableContent, true),
		};
		state.patch(node, result);
		return state.applyData(node, result);
	}
};

export function readFrontmatter(source: string) {
	const frontmatterProcessor: Plugin<[], Mdast.Root, Record<string, unknown>> = function (this: Processor) {
		Object.assign(this, { Compiler: ((function compiler(root: Mdast.Root) {
			if ('children' in root && Array.isArray(root.children) && root.children.length !== 0 && 'type' in root.children[0] && root.children[0].type === 'yaml')
				return parseYaml(root.children[0].value, { customTags: ['timestamp'] }) as Record<string, unknown>;
			return {};
		}) as unknown as CompilerFunction<Hast.Node, Record<string, unknown>>) });
	};

	return unified().use(remarkParse).use(remarkFrontmatter).use(frontmatterProcessor).processSync(source).result as Record<string, unknown>;
}

const fixFootnoteLabels: Plugin<[], Hast.Root> = () => (root: Hast.Root) => {
	unistVisit(root, 'element', (node: Hast.Element) => {
		if ('properties' in node && 'dataFootnoteBackref' in (node.properties ?? {})) {
			const s = node.children[1];
			node.properties ??= {};
			node.properties.style = 'text-decoration-color: transparent;';
			node.children = [{
				type: 'element',
				tagName: 'span',
				properties: {
					class: 'ms-1 link-underline link-underline-opacity-0 link-body-emphasis',
				},
				children: [{
					type: 'element',
					tagName: 'i',
					properties: {
						class: 'bi bi-arrow-up-left',
						ariaHidden: true,
					},
					children: [],
				} as Hast.Element, s].filter(Boolean),
			}]
		}
	});
}

export function renderMarkdown(source: string) {
	const jsx: Jsx = (type, props, key) => {
		const children = props.children === undefined ? [] : Array.isArray(props.children) ? props.children : [props.children];
		return h(type as string | CallableFunction, {
			...Object.fromEntries(Object.entries(props).filter(([k]) => k !== 'children')),
			...'style' in props && { style: Object.entries(props.style as Record<string, unknown>).map(([k, v]) => k + ':' + v).join('; ') },
			_jsx_key: key
		}, ...children.filter(child => child !== undefined && child !== null));
	}

	const options: ToJsxRuntimeOptions = {
		jsx,
		jsxs: jsx,
		Fragment,
		development: false,
		elementAttributeNameCase: 'html',
		stylePropertyNameCase: 'css',
		createEvaluater: () => ({
			evaluateExpression: expr => Expression.evaluate.sync(expr),
			evaluateProgram: prog => Expression.evaluate.sync(prog),
		}),
		components: {
			primitive: PrimitiveLink,
			combinator: CombinatorDiagram,
			deprecated: DeprecatedAlert,
			planned: PlannedAlert,
		} as unknown as Components,
	};

	const toJsx: Plugin<[], Hast.Root, JSX.Element> = function (this: Processor) {
		Object.assign(this, { Compiler: ((function compiler(root: Hast.Root) {
			return toJsxRuntime(root as unknown as Parameters<typeof toJsxRuntime>[0], options);
		}) as unknown as CompilerFunction<Hast.Node, JSX.Element>) });
	}

	const plugMdx: Plugin<[], Mdast.Root, Mdast.Root> = function (this: Processor) {
		const data = this.data();
		data.micromarkExtensions ??= [];
		data.fromMarkdownExtensions ??= [];
		data.toMarkdownExtensions ??= [];
		(data.micromarkExtensions as unknown[]).push(mdxjs({ acorn, addResult: true }));
		(data.fromMarkdownExtensions as unknown[]).push(mdxFromMarkdown());
		(data.toMarkdownExtensions as unknown[]).push(mdxToMarkdown());
	}

	return unified()
		.use(remarkParse)
		.use(plugMdx)
		.use(remarkGfm)
		.use(remarkFrontmatter)
		.use(remarkMath)
		.use(remarkRehype, null, {
			handlers,
			allowDangerousHtml: true,
			passThrough: [ 'mdxJsxFlowElement', 'mdxJsxTextElement', 'mdxFlowExpression', 'mdxTextExpression', 'mdxjsEsm' ],
		})
		.use(fixFootnoteLabels)
		.use(rehypeKatex as unknown as Plugin<[], Hast.Root>)
		.use(rehypeFormat)
		.use(toJsx)
		.processSync(source)
		.result as JSX.Element;
}

