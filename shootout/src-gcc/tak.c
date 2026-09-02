extern void *bmperf_alloc(void);
extern void bmperf_snap(void *);
extern void bmperf_report(void *, void *);
/* tak, C: Takeuchi over ints at 9 6 3, answer 6. Mirrors benches/tak.ml. */
#include <stdio.h>
static int tak(int x, int y, int z) {
    if (y >= x) return z;
    return tak(tak(x-1,y,z), tak(y-1,z,x), tak(z-1,x,y));
}
static int bench(void) { printf("%d\n", tak(9,6,3)); return 0; }

int main(void) {
    void *s = bmperf_alloc(), *e = bmperf_alloc();
    bmperf_snap(s);
    int r = bench();
    bmperf_snap(e);
    bmperf_report(s, e);
    return r;
}
