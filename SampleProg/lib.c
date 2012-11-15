#include <stdio.h>

void putInt(long i) {
    printf("%ld\n", i);
}

void putHex(long i) {
    printf("%lx\n", i);
}

long get7thArg(long a, long b, long c,
               long d, long e, long f,
               long g) {
    return g;
}

long get8thArg(long a, long b, long c,
               long d, long e, long f,
               long g, long h) {
    return h;
}
