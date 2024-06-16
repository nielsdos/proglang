# RUN: ../compile %s |& FileCheck %s

fn if_check() -> int:
    if 0:
        return 1
    return 0

# CHECK: {{.+}}: expected a condition of type 'bool', but this condition has type 'int'

fn while_loop() -> int:
    while 0:
        return 1
    return 0

# CHECK: {{.+}}: expected a condition of type 'bool', but this condition has type 'int'
