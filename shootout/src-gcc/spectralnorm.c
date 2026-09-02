extern void *bmperf_alloc(void);
extern void bmperf_snap(void *);
extern void bmperf_report(void *, void *);
/* spectralnorm, C: n = 8, ten power iterations, answer 1270046208. */
#include <stdio.h>
#include <math.h>
#define N 8
static float uu[N], v[N], t[N];
static float aij(int i, int j) { int s = i + j; return 1.0f / (float)(s*(s+1)/2 + i + 1); }
static void av(float *x, float *y)  { int i,j; for (i=0;i<N;i++){ float a=0.0f; for(j=0;j<N;j++) a += aij(i,j)*x[j]; y[i]=a; } }
static void atv(float *x, float *y) { int i,j; for (i=0;i<N;i++){ float a=0.0f; for(j=0;j<N;j++) a += aij(j,i)*x[j]; y[i]=a; } }
static void atav(float *x, float *y) { av(x,t); atv(t,y); }
static int bench(void) {
    int i,k; float vbv=0.0f, vv=0.0f;
    for (i=0;i<N;i++){ uu[i]=1.0f; v[i]=0.0f; }
    for (k=0;k<10;k++){ atav(uu,v); atav(v,uu); }
    for (i=0;i<N;i++){ vbv += uu[i]*v[i]; vv += v[i]*v[i]; }
    printf("%d\n", (int)(sqrtf(vbv/vv) * 1000000000.0f));
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
