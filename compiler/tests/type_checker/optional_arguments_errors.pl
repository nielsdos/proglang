# RUN: ../compile %s |& FileCheck %s

fn test(int a, int b, int c = 3 * 4, int d = 4) -> int:
    return a - b * c - d

fn caller1() -> int:
    return test(b = 2, a = 1)

fn caller2() -> int:
    return test(1)

# CHECK: {{.+}}: argument 'b' not passed

fn caller3() -> int:
    return test(b = 1)

# CHECK-NEXT: argument 'a' not passed

fn caller4() -> int:
    return test(c = 3, d = 4)

# CHECK-NEXT: argument 'a' not passed
# CHECK-NEXT: argument 'b' not passed

fn caller5() -> int:
    return test(1, c = 3)

# CHECK-NEXT: argument 'b' not passed

fn caller6() -> int:
    return test(1, 2, 3, 4)

fn caller7() -> int:
    return test()

# CHECK-NEXT: argument 'a' not passed
# CHECK-NEXT: argument 'b' not passed

fn caller8() -> int:
    return test(1, 2, 3, 4, 5, 6)

# CHECK-NEXT: expected at most 4 arguments, but this function call has 6 arguments
