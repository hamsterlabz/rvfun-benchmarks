extern void *bmperf_alloc(void);
extern void bmperf_snap(void *);
extern void bmperf_report(void *, void *);
/* alloc, C: the suite's allocation stress test, 20 x 1000 objects, answer
   120000. The OCaml allocates a 3-element array per object; C uses malloc so
   the allocation is real rather than optimised away. */
#include <stdio.h>
#include <stdlib.h>
static int create(int n, int acc) {
    while (n > 0) {
        int *o = (int *)malloc(3 * sizeof(int));
        o[0] = 5; o[1] = n; o[2] = 1;
        acc += o[0] + o[2];
        free(o);
        n--;
    }
    return acc;
}
static int bench(void) {
    int i, acc = 0;
    for (i = 20; i > 0; i--) acc += create(1000, 0);
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
