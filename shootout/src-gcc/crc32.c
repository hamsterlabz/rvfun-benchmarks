extern void *bmperf_alloc(void);
extern void bmperf_snap(void *);
extern void bmperf_report(void *, void *);
/* crc32, C: standard reflected CRC-32 over an LCG byte stream, n = 2000,
   answer 689943297 (matches python zlib.crc32). */
#include <stdio.h>
#define N 2000
static unsigned int tbl[256];
static int bench(void) {
    unsigned int i, c, seed = 1, k;
    for (i = 0; i < 256; i++) {
        c = i;
        for (k = 0; k < 8; k++) c = (c & 1) ? (3988292384u ^ (c >> 1)) : (c >> 1);
        tbl[i] = c;
    }
    c = 4294967295u;
    for (i = 0; i < N; i++) {
        unsigned int s = (seed * 3877u + 29573u) % 139968u, b = s & 255u;
        c = tbl[(c ^ b) & 255u] ^ ((c >> 8) & 16777215u);
        seed = s;
    }
    printf("%d\n", (int)(c ^ 4294967295u));
    return 0;
}

int main(void) {
    void *s = bmperf_alloc(), *e = bmperf_alloc();
    bmperf_snap(s);
    int r = bench();
    bmperf_snap(e);
    bmperf_report(s, e);
    return r;
}
