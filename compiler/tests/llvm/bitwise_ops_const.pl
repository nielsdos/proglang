# RUN: ../compile %s |& FileCheck %s

fn bnot(int x): int
    return ~x
end

fn bor(int x): int
    return x | 1
end

fn band(int x): int
    return x & 1
end

fn lshift(int x): int
    return x << 1
end

fn rshift(int x): int
    return x >> 1
end

fn rashift(int x): int
    return x >>> 1
end

# CHECK-DAG: %not = xor i64 %1, -1
# CHECK-DAG: %or = or i64 %1, 1
# CHECK-DAG: %and = and i64 %1, 1
# CHECK-DAG: %rshift = lshr i64 %1, 1
# CHECK-DAG: %lshift = shl i64 %1, 1
# CHECK-DAG: %rshift = ashr i64 %1, 1
