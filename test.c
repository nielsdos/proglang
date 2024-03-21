#include <stdio.h>
#include <stdint.h>

extern int64_t factorial(int64_t);

int main() {
	printf("%ld\n", factorial(6));
	return 0;
}
