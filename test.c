#include <stdio.h>
#include <stdint.h>

extern double test_double(void);
extern int64_t test_int(void);

int main() {
	printf("%g %ld\n", test_double(), test_int());
	return 0;
}
