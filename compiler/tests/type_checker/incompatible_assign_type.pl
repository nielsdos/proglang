# RUN: ../compile %s |& FileCheck %s

fn foo()
  mut a = 1
  a = false
end

# CHECK: {{.+}}: the variable 'a' has mismatching types: previously had type 'int', but this assigns a value of type 'bool'
