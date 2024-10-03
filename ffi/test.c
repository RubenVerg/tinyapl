#include "stdio.h"

#ifdef _WIN32
#define EXPORT __declspec(dllexport)
#else
#define EXPORT
#endif

EXPORT void printHello() {
	printf("Hello from C!\n");
}

EXPORT int add(int a, int b) {
	return a + b;
}