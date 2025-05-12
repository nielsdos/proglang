# RUN: ../compile %s |& FileCheck %s

class A:
    B b

class B:
    C c

class C:
    D d

class D:
    B b
    C c

# CHECK: {{.+}}: This field will cause a struct cycle, which leads to an infinitely nested type. Break the cycle using a class instead of a struct type. Checked: {{.+}}
