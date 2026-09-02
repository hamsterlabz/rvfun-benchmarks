extern void *bmperf_alloc(void); extern void bmperf_snap(void *);
extern void bmperf_report(void *, void *);
/* matrix, C: shootout matrix -- 20x20 integer multiply, 3 passes, reporting
   mm[0][0] + mm[2][3] + mm[3][2] + mm[4][4] as the shootout entry does. */
#include <stdio.h>
#define SZ 20
static long m1[SZ][SZ], m2[SZ][SZ], mm[SZ][SZ];
static int bench(void) {
    long i, j, k, n, c = 1;
    for (i = 0; i < SZ; i++) for (j = 0; j < SZ; j++) { m1[i][j] = c; m2[i][j] = c; c++; }
    for (n = 0; n < 3; n++)
        for (i = 0; i < SZ; i++)
            for (j = 0; j < SZ; j++) {
                long s = 0;
                for (k = 0; k < SZ; k++) s += m1[i][k] * m2[k][j];
                mm[i][j] = s;
            }
    printf("%ld\n", mm[0][0] + mm[2][3] + mm[3][2] + mm[4][4]); return 0;
}
int main(void){void*s=bmperf_alloc(),*e=bmperf_alloc();bmperf_snap(s);int r=bench();bmperf_snap(e);bmperf_report(s,e);return r;}
