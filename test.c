#include <stdio.h>

extern double test(void);

int main() {
	printf("%g\n", test());
	return 0;
}
