extern void *bmperf_alloc(void); extern void bmperf_snap(void *);
extern void bmperf_report(void *, void *);
/* sieve, C: shootout sieve -- Eratosthenes to 4096, 3 passes, prime count. */
#include <stdio.h>
#define LIM 4096
static char f[LIM+1];
static int bench(void) {
    long n, i, j, count = 0;
    for (n = 0; n < 3; n++) {
        count = 0;
        for (i = 2; i <= LIM; i++) f[i] = 1;
        for (i = 2; i <= LIM; i++)
            if (f[i]) { for (j = i + i; j <= LIM; j += i) f[j] = 0; count++; }
    }
    printf("%ld\n", count); return 0;
}
int main(void){void*s=bmperf_alloc(),*e=bmperf_alloc();bmperf_snap(s);int r=bench();bmperf_snap(e);bmperf_report(s,e);return r;}
