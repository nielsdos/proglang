# RUN: ../compile %s |& FileCheck %s

fn foo():
  let a = 1
  a = 0

# CHECK: {{.+}}: the variable 'a' is immutable
