extern void *bmperf_alloc(void);
extern void bmperf_snap(void *);
extern void bmperf_report(void *, void *);
/* takfp, C, mirroring benches/takfp.ml: Takeuchi over floats at 18 12 6,
   answer 7. */
#include <stdio.h>
static float tak(float x, float y, float z) {
    if (y >= x) return z;
    return tak(tak(x - 1.0f, y, z), tak(y - 1.0f, z, x), tak(z - 1.0f, x, y));
}
static int bench(void) {
    printf("%d\n", (int)tak(18.0f, 12.0f, 6.0f));
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
