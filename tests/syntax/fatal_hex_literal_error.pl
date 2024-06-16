# RUN: ../compile %s |& FileCheck %s

fn hex() -> int:
    return 0xaz

# CHECK: {{.+}}: found z expected '(', '.', '**', '*', '/', '//', '+', '-', '<', '>', '==', '!=', '<=', '>=', or 'end of statement'
