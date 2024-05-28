export interface Info {
	name: string;
	planned: boolean;
	body: string;
}

export interface Primitive {
	glyph: string;
	pattern: string;
	name: string;
	planned: boolean;
	deprecated: boolean;
	body: string;
}

export interface Quad {
	glyph: string;
	pattern: string;
	name: string;
	category: string;
	planned: boolean;
	deprecated: boolean;
	body: string;
}

export interface Pages {
	index: string;
	info: Record<string, Info>;
	primitives: Record<string, Primitive>;
	quads: Record<string, Quad>;
}