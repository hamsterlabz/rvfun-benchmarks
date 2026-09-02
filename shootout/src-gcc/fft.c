extern void *bmperf_alloc(void);
extern void bmperf_snap(void *);
extern void bmperf_report(void *, void *);
/* fft, C: radix-2 decimation-in-time, n = 32, checksum/1000, answer 0. */
#include <stdio.h>
#include <math.h>
#define N 32
#define HALF 16
static float xre[N], xim[N], yre[N], yim[N], wre[HALF], wim[HALF];
static int bitrev(int acc, int nbits, int i) { while (nbits--) { acc = acc*2 + (i%2); i /= 2; } return acc; }
static int bench(void) {
    int i, j, k, m; float acc = 0.0f;
    for (i = 0; i < N; i++) { xre[i] = sinf((float)i); xim[i] = 0.0f; }
    for (k = 0; k < HALF; k++) {
        float a = (0.0f - 6.28318530717958f) * (float)k / (float)N;
        wre[k] = cosf(a); wim[k] = sinf(a);
    }
    for (i = 0; i < N; i++) { j = bitrev(0, 5, i); yre[i] = xre[j]; yim[i] = xim[j]; }
    for (m = 2; m <= N; m *= 2) {
        int hm = m/2, step = N/m, ofs;
        for (ofs = 0; ofs < N; ofs += m)
            for (i = 0; i < hm; i++) {
                int jj = ofs+i, kk = jj+hm, tw = i*step;
                float br=yre[kk], bi=yim[kk], cr=wre[tw], ci=wim[tw];
                float pr = br*cr - bi*ci, pi = br*ci + bi*cr;
                float ar = yre[jj], ai = yim[jj];
                yre[jj]=ar+pr; yim[jj]=ai+pi; yre[kk]=ar-pr; yim[kk]=ai-pi;
            }
    }
    for (i = 0; i < N; i++) acc += yre[i]*yre[i] + yim[i]*yim[i];
    printf("%d\n", (int)(acc/1000.0f));
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
