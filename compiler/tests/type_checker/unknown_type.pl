# RUN: ../compile %s |& FileCheck %s

fn foo1(Foo f):
  return

fn foo2() -> Foo:
  return

# CHECK: {{.+}}: type 'Foo' was not found
# CHECK: {{.+}}: type 'Foo' was not found
# CHECK: {{.+}}: function must return a value of type 'Foo', but this returns nothing
