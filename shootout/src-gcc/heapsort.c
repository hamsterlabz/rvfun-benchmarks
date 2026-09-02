extern void *bmperf_alloc(void);
extern void bmperf_snap(void *);
extern void bmperf_report(void *, void *);
/* heapsort, C, mirroring benches/heapsort.ml: LCG fill, sift-down, weighted
   checksum. n = 100, answer 486046838. */
#include <stdio.h>
#define N 100
static int a[N];
static void sift(int root, int last) {
    for (;;) {
        int c = 2 * root + 1, c2, t;
        if (c > last) return;
        c2 = (c + 1 <= last && a[c + 1] > a[c]) ? c + 1 : c;
        if (a[c2] > a[root]) { t = a[root]; a[root] = a[c2]; a[c2] = t; root = c2; }
        else return;
    }
}
static int bench(void) {
    int i, seed = 1, e, acc = 0;
    for (i = 0; i < N; i++) { seed = (seed * 3877 + 29573) % 139968; a[i] = seed; }
    for (i = N / 2 - 1; i >= 0; i--) sift(i, N - 1);
    for (e = N - 1; e > 0; e--) { int t = a[0]; a[0] = a[e]; a[e] = t; sift(0, e - 1); }
    for (i = 0; i < N; i++) acc = (acc + a[i] * (i + 1)) % 1000000007;
    printf("%d\n", acc);
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
