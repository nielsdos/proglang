# RUN: ../compile %s |& FileCheck %s

class Foo:
  int x

fn foo(Foo f) -> int:
  return f.y

# CHECK: {{.+}}: field 'y' does not exist in type 'Foo'
