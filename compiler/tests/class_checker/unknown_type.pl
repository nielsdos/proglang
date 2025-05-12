# RUN: ../compile %s |& FileCheck %s

class A:
    Foo f

# CHECK: {{.+}}: The type 'Foo' was not found
