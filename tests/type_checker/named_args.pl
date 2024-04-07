# RUN: ../compile %s |& FileCheck %s

fn test(int a, int b) -> int:
    return a - b

fn caller1() -> int:
    return test(b=1, a = 3)

# CHECK: Note: {{.+}}: first named argument passed here
# CHECK-NEXT: {{.+}}: positional arguments must not come after named arguments

fn caller2() -> int:
    return test(b = 1, 3)

# CHECK-NEXT: {{.+}}: named arguments are not supported in indirect calls

fn caller3() -> int:
    let t = test
    return t(b = 1, 3)

# CHECK-NEXT: {{.+}}: argument 'c' not found in function 'test'

fn caller4() -> int:
    return test(c = 1, d = 3)

# CHECK-NEXT: {{.+}}: argument 'd' not found in function 'test'

fn caller5() -> int:
    return test(a = 1, a = 3)

# CHECK-NEXT: {{.+}}: argument 'a' already passed
