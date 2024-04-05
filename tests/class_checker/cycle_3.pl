# RUN: ../compile %s |& FileCheck %s

class A:
    A a

# CHECK: {{.+}}: This field will cause a struct cycle, which leads to an infinitely nested type. Break the cycle using a class instead of a struct type. Checked: {{.+}}
