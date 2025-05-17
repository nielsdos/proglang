# RUN: ../compile %s |& FileCheck %s

fn test(int a, int b, int c = 3 * 4, int d = 4): int
    return a - b * c - d
end

fn caller1(): int
    return test(1, 2)
end

# CHECK-DAG: define i64 @caller1(ptr nocapture readnone %0) local_unnamed_addr #{{[0-9]}} {
# CHECK-DAG: entry:
# CHECK-DAG:   ret i64 -27
# CHECK-DAG: }

fn caller2(): int
    return test(b = 2, a = 1)
end

# CHECK-DAG: define i64 @caller2(ptr nocapture readnone %0) local_unnamed_addr #{{[0-9]}} {
# CHECK-DAG: entry:
# CHECK-DAG:   ret i64 -27
# CHECK-DAG: }

fn caller3(): int
    return test(d = 5, c = 1, b = 2, a = 1)
end

# CHECK-DAG: define i64 @caller3(ptr nocapture readnone %0) local_unnamed_addr #{{[0-9]}} {
# CHECK-DAG: entry:
# CHECK-DAG:   ret i64 -6
# CHECK-DAG: }

fn caller4(): int
    return test(1, 2, d = 0)
end

# CHECK-DAG: define i64 @caller4(ptr nocapture readnone %0) local_unnamed_addr #{{[0-9]}} {
# CHECK-DAG: entry:
# CHECK-DAG:   ret i64 -23
# CHECK-DAG: }

fn caller5(): int
    return test(1, 2, c = 0)
end

# CHECK-DAG: define i64 @caller5(ptr nocapture readnone %0) local_unnamed_addr #{{[0-9]}} {
# CHECK-DAG: entry:
# CHECK-DAG:   ret i64 -3
# CHECK-DAG: }
