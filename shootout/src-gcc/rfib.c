extern void *bmperf_alloc(void);
extern void bmperf_snap(void *);
extern void bmperf_report(void *, void *);
/* rfib, C: nfib over floats at 11, answer 287. Mirrors benches/rfib.ml. */
#include <stdio.h>
static float nfib(float n) {
    if (n <= 1.0f) return 1.0f;
    return nfib(n-1.0f) + nfib(n-2.0f) + 1.0f;
}
static int bench(void) { printf("%d\n", (int)nfib(11.0f)); return 0; }

int main(void) {
    void *s = bmperf_alloc(), *e = bmperf_alloc();
    bmperf_snap(s);
    int r = bench();
    bmperf_snap(e);
    bmperf_report(s, e);
    return r;
}
