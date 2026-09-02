extern void *bmperf_alloc(void);
extern void bmperf_snap(void *);
extern void bmperf_report(void *, void *);
/* floydwarshall, C: n = 12, LCG-filled weights, answer 2918424. */
#include <stdio.h>
#define N 12
#define INF 100000000
static int d[N][N];
static int bench(void) {
    int i, j, k, seed = 1, acc = 0;
    for (i = 0; i < N; i++) for (j = 0; j < N; j++) d[i][j] = INF;
    for (i = 0; i < N; i++)
        for (j = 0; j < N; j++) {
            int s = (seed * 3877 + 29573) % 139968;
            if (i == j) d[i][j] = 0; else if (s < 46656) d[i][j] = s + 1;
            seed = s;
        }
    for (k = 0; k < N; k++)
        for (i = 0; i < N; i++)
            for (j = 0; j < N; j++) {
                int t = d[i][k] + d[k][j];
                if (t < d[i][j]) d[i][j] = t;
            }
    for (i = 0; i < N; i++) for (j = 0; j < N; j++) if (d[i][j] < INF) acc += d[i][j];
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
