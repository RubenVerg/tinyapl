export interface Glyph {
	glyph: string;
	name: string;
	primitives: string[];
	body: JSX.Element;
}

export interface Info {
	name: string;
	planned: boolean;
	body: JSX.Element;
}

export interface Primitive {
	glyph: string;
	pattern: string;
	name: string;
	planned: boolean;
	deprecated: boolean;
	body: JSX.Element;
}

export interface Quad {
	glyph: string;
	pattern: string;
	name: string;
	category: string;
	planned: boolean;
	deprecated: boolean;
	body: JSX.Element;
}

export interface Pages {
	index: JSX.Element;
	glyphs: Record<string, Glyph>;
	info: Record<string, Info>;
	primitives: Record<string, Primitive>;
	quads: Record<string, Quad>;
}

export type Interpreters = string[];
