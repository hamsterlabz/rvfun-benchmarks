extern void *bmperf_alloc(void); extern void bmperf_snap(void *);
extern void bmperf_report(void *, void *);
/* nestedloop, C: shootout nestedloop, n = 6 -> 6^6 = 46656. */
#include <stdio.h>
#define NN 6
static int bench(void) {
    long a=0,i,j,k,l,m,o;
    for(i=0;i<NN;i++)for(j=0;j<NN;j++)for(k=0;k<NN;k++)
    for(l=0;l<NN;l++)for(m=0;m<NN;m++)for(o=0;o<NN;o++) a++;
    printf("%ld\n", a); return 0;
}
int main(void){void*s=bmperf_alloc(),*e=bmperf_alloc();bmperf_snap(s);int r=bench();bmperf_snap(e);bmperf_report(s,e);return r;}
