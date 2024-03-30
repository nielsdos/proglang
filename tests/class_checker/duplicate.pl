# RUN: ../compile %s |& filecheck %s

class Foo:
    int x
    int y

class Foo:
    int z

# CHECK: {{.+}}: previously declared here
# CHECK-NEXT: the class 'Foo' was already declared
