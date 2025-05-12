#include <stdio.h>
#include <stdint.h>

extern int32_t rt_test(int32_t);

int main() {
	printf("%d\n", rt_test(4));
	return 0;
}
