export interface Info {
	name: string;
	planned: boolean;
	body: string;
}

export interface Primitive {
	glyph: string
	pattern: string;
	name: string;
	planned: boolean;
	body: string;
}

export interface Pages {
	info: Record<string, Info>;
	primitives: Record<string, Primitive>;
}