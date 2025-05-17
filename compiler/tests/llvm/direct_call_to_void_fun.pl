# RUN: ../compile --optimization-level=0 %s |& FileCheck %s

fn voidfun()
    return
end

# CHECK-DAG: define void @voidfun(ptr %0) {
# CHECK-DAG: entry:
# CHECK-DAG:   ret void
# CHECK-DAG: }

fn nonvoidfun(): int
    voidfun()
    return 1
end

# CHECK-DAG: define i64 @nonvoidfun(ptr %0) {
# CHECK-DAG: entry:
# CHECK-DAG:   call void @voidfun(ptr %0)
# CHECK-DAG:   ret i64 1
# CHECK-DAG: }
