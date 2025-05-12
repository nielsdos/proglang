# RUN: ../compile %s |& FileCheck %s

fn hex() -> int:
    return 0b12

# CHECK: {{.+}}: found 2 expected '(', '.', '**', '*', '/', '//', '+', '-', '<', '>', '==', '!=', '<=', '>=', or 'end of statement'
