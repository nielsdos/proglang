#include <stdio.h>
#include <stdint.h>

extern double test_double(int64_t);
extern int64_t test_int(int64_t, int64_t);

int main() {
	printf("%g %ld\n", test_double(0), test_int(2, 8));
	return 0;
}
