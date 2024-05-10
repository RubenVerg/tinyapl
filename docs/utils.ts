export function recordGetter(o: any) {
	return { get<A>(p: string): A | undefined { return o[p] as A; } }
}