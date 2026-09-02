extern void *bmperf_alloc(void); extern void bmperf_snap(void *);
extern void bmperf_report(void *, void *);
/* random, C: the shootout LCG (IM 139968, IA 3877, IC 29573), 20000 draws,
   reporting the final seed. */
#include <stdio.h>
#define IM 139968
#define IA 3877
#define IC 29573
static int bench(void) {
    long seed = 42, i;
    for (i = 0; i < 20000; i++) seed = (seed * IA + IC) % IM;
    printf("%ld\n", seed); return 0;
}
int main(void){void*s=bmperf_alloc(),*e=bmperf_alloc();bmperf_snap(s);int r=bench();bmperf_snap(e);bmperf_report(s,e);return r;}
