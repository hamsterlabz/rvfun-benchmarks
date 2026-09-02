extern void *bmperf_alloc(void);
extern void bmperf_snap(void *);
extern void bmperf_report(void *, void *);
/* mandelbrot, C, mirroring benches/mandelbrot.ml: n x n grid on
   [-1.5,0.5]x[-1,1], fifty iterations, population count. n = 12, answer 60. */
#include <stdio.h>
#define N 12
static int inside(float cr, float ci) {
    float zr = 0.0f, zi = 0.0f;
    int k;
    for (k = 0; k < 50; k++) {
        float nzr;
        if (zr * zr + zi * zi > 4.0f) return 0;
        nzr = zr * zr - zi * zi + cr;
        zi  = 2.0f * zr * zi + ci;
        zr  = nzr;
    }
    return 1;
}
static int bench(void) {
    int i, j, acc = 0;
    for (i = 0; i < N; i++)
        for (j = 0; j < N; j++)
            acc += inside(2.0f * j / (float)N - 1.5f, 2.0f * i / (float)N - 1.0f);
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
