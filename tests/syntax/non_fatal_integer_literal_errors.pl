# RUN: ../compile %s |& FileCheck %s

fn leading_zero() -> int:
    return 01

# CHECK: {{.+}}: leading zeros in integer literals are not allowed, use an 0o prefix for octal integers

fn overflow() -> int:
    return 99999999999999999999999999999999999999999999999999999999999999999999999999999999999999

# CHECK-NEXT: {{.+}}: integer literal overflow
