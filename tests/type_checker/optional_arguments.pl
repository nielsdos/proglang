# RUN: ../compile %s |& FileCheck %s

fn test(int a, int b, int c = 3 * 4, int d = 4.0, int e) -> int:
    return a - b * c

# CHECK: {{.+}}: default value of argument 'd' has type 'float' but argument has type 'int'
# CHECK-NEXT: Note: {{.+}}: first optional argument declared here
# CHECK-NEXT: {{.+}}: non-optional arguments must come before the first optional argument
