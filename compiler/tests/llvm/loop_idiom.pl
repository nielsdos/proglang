# RUN: ../compile %s |& FileCheck %s

fn loop_idiom(): int
    mut sum = 0
    mut i = 0
    while i <= 10 do
        sum = sum + i
        i = i + 1
    end
    return sum
end

# CHECK: define i64 @loop_idiom(ptr nocapture readnone %0) local_unnamed_addr #{{[0-9]+}} {
# CHECK-NEXT: after_loop:
# CHECK-NEXT:   ret i64 55
# CHECK-NEXT: }
