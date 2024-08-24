#include <Rts.h>

#include "TinyAPL/StandardLibrary_stub.h"

#include <stdio.h>

void __wizer_initialize_stub(void) {
	hs_init(NULL, NULL);
	loadStandardLibrary();
	hs_perform_gc();
	hs_perform_gc();
	rts_clearMemory();
}
