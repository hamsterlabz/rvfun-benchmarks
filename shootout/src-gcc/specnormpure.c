extern void *bmperf_alloc(void); extern void bmperf_snap(void *);
extern void bmperf_report(void *, void *);
/* spectralnorm-pure, C: the SAME computation as spectralnorm.c (n = 8, ten
   power iterations, answer 1270046208) with NO arrays and NO mutable state --
   a vector is a singly linked list of float and every step allocates a fresh
   one.  The row compares the representation, not the language: the shootout
   entry writes through three static buffers, this one rebuilds cons cells the
   way the Haskell version does. */
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#define N 8
typedef struct vec { float hd; struct vec *tl; } vec;

static vec *cons(float h, vec *t) {
    vec *v = (vec *)malloc(sizeof(vec)); v->hd = h; v->tl = t; return v;
}
static float aij(int i, int j) { int s = i + j; return 1.0f / (float)(s*(s+1)/2 + i + 1); }
/* one row of A . x  /  A^T . x -- walk the list, no indexing */
static float rowDot(int i, vec *x) {
    float a = 0.0f; int j = 0;
    for (; x != NULL; x = x->tl, j++) a += aij(i, j) * x->hd;
    return a;
}
static float colDot(int i, vec *x) {
    float a = 0.0f; int j = 0;
    for (; x != NULL; x = x->tl, j++) a += aij(j, i) * x->hd;
    return a;
}
static vec *av(vec *x)  { vec *r = NULL; int i; for (i = N-1; i >= 0; i--) r = cons(rowDot(i, x), r); return r; }
static vec *atv(vec *x) { vec *r = NULL; int i; for (i = N-1; i >= 0; i--) r = cons(colDot(i, x), r); return r; }
static vec *atav(vec *x) { return atv(av(x)); }
static float zipDot(vec *a, vec *b) {
    float s = 0.0f;
    for (; a != NULL && b != NULL; a = a->tl, b = b->tl) s += a->hd * b->hd;
    return s;
}
static int bench(void) {
    vec *u = NULL, *v; int i, k;
    for (i = 0; i < N; i++) u = cons(1.0f, u);
    for (k = 0; k < 9; k++) u = atav(atav(u));
    v = atav(u);
    u = atav(v);
    printf("%d\n", (int)(sqrtf(zipDot(u, v) / zipDot(v, v)) * 1000000000.0f));
    return 0;
}
int main(void){void*s=bmperf_alloc(),*e=bmperf_alloc();bmperf_snap(s);int r=bench();bmperf_snap(e);bmperf_report(s,e);return r;}
