extern void __wizer_initialize_stub(void);

#include <stdio.h>

__attribute__((export_name("wizer.initialize"))) void __wizer_initialize(void) {
	__wizer_initialize_stub();
}