# RUN: ../compile %s |& filecheck %s

fn foo():
  mut a = 1
  a = false

# CHECK: {{.+}}: the variable 'a' has mismatching types: previously had type 'int', but this assigns a value of type 'bool'
