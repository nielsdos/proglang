# RUN: ../compile %s |& FileCheck %s

fn hex() -> int:
    return 0o19

# CHECK: {{.+}}: found 9 expected '(', '.', '**', '*', '/', '//', '+', '-', '<', '>', '==', '!=', '<=', '>=', or 'end of statement'
