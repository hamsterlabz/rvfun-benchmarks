extern void *bmperf_alloc(void); extern void bmperf_snap(void *);
extern void bmperf_report(void *, void *);
/* quadtree, C: the gadt QuadTreePure benchmark.  A 4-way addressed tree, one
   malloc'd node per quadrant with four named child pointers (NO arrays), and
   path-copying insert/delete over an 8-bit MSB-first address (depth 4).
   W is uint32_t.  Answer 1255315216. */
#include <stdio.h>
#include <stdlib.h>
typedef unsigned int W;
typedef struct qt { W v; struct qt *k0, *k1, *k2, *k3; } qt;   /* NULL = QtEmpty */
#define QTDEPTH 4
#define INSERTCOUNT 24

static qt *mk(W v, qt *a, qt *b, qt *c, qt *d) {
    qt *n = (qt *)malloc(sizeof(qt));
    n->v = v; n->k0 = a; n->k1 = b; n->k2 = c; n->k3 = d; return n;
}
typedef struct pl { W a, b; struct pl *tl; } pl;
static pl *pcons(W a, W b, pl *t) { pl *p = (pl *)malloc(sizeof(pl)); p->a = a; p->b = b; p->tl = t; return p; }
/* mkOps n s = (s1 .&. 0xFF, s2) : mkOps (n-1) s2 */
static pl *mkOps(int n, W s) {
    if (n <= 0) return NULL;
    { W s1 = s * 1664525u + 1013904223u, s2 = s1 * 1664525u + 1013904223u;
      return pcons(s1 & 0xFFu, s2, mkOps(n - 1, s2)); }
}
static W lcgNext(W s) { return s * 1664525u + 1013904223u; }
static W hashMix(W h, W v) {
    W a = h ^ v, b = a * 0x85ebca6bu, c = b ^ (b >> 13), d = c * 0xc2b2ae35u;
    return d ^ (d >> 16);
}
static qt *kidOf(qt *t, int q) {
    if (t == NULL) return NULL;
    if (q == 0) return t->k0;
    if (q == 1) return t->k1;
    if (q == 2) return t->k2;
    return t->k3;
}
/* rebuild t's kid set with kid q replaced; t may be QtEmpty */
static qt *withKid(qt *t, int q, qt *nk, W v) {
    qt *a = kidOf(t, 0), *b = kidOf(t, 1), *c = kidOf(t, 2), *d = kidOf(t, 3);
    if (q == 0) a = nk; else if (q == 1) b = nk; else if (q == 2) c = nk; else d = nk;
    return mk(v, a, b, c, d);
}
static qt *qtInsertAt(qt *t, W addr, int dl, W v) {
    int q; qt *nk;
    if (dl == 0) return mk(v, kidOf(t,0), kidOf(t,1), kidOf(t,2), kidOf(t,3));
    q  = (int)((addr >> ((dl - 1) * 2)) & 0x3u);
    nk = qtInsertAt(kidOf(t, q), addr, dl - 1, v);
    return withKid(t, q, nk, t == NULL ? 0u : t->v);
}
static qt *qtInsert(qt *t, W addr, W v) { return qtInsertAt(t, addr, QTDEPTH, v); }
static qt *qtDeleteAt(qt *t, W addr, int dl) {
    int q; qt *nk;
    if (t == NULL) return NULL;
    if (dl == 0) return mk(0u, t->k0, t->k1, t->k2, t->k3);
    q  = (int)((addr >> ((dl - 1) * 2)) & 0x3u);
    nk = qtDeleteAt(kidOf(t, q), addr, dl - 1);
    return withKid(t, q, nk, t->v);
}
static qt *qtDelete(qt *t, W addr) { return qtDeleteAt(t, addr, QTDEPTH); }
/* qtFoldl f z (QtNode v a b c d) = foldl (qtFoldl f) (f z v) [a,b,c,d] */
static W qtFoldlAdd(W z, qt *t) {
    if (t == NULL) return z;
    z = z + t->v;
    z = qtFoldlAdd(z, t->k0); z = qtFoldlAdd(z, t->k1);
    z = qtFoldlAdd(z, t->k2); z = qtFoldlAdd(z, t->k3);
    return z;
}
/* qtFoldr f z (QtNode v a b c d) = f v (foldr (flip (qtFoldr f)) z [a,b,c,d]) */
static W qtFoldrXor(W z, qt *t) {
    if (t == NULL) return z;
    z = qtFoldrXor(z, t->k3); z = qtFoldrXor(z, t->k2);
    z = qtFoldrXor(z, t->k1); z = qtFoldrXor(z, t->k0);
    return t->v ^ z;
}
static qt *qtMap3p1(qt *t) {
    if (t == NULL) return NULL;
    return mk(t->v * 3u + 1u, qtMap3p1(t->k0), qtMap3p1(t->k1),
                              qtMap3p1(t->k2), qtMap3p1(t->k3));
}
static W qtZipSumAdd(qt *a, qt *b) {
    if (a == NULL || b == NULL) return 0u;
    return (a->v + b->v) + qtZipSumAdd(a->k0, b->k0) + qtZipSumAdd(a->k1, b->k1)
                         + qtZipSumAdd(a->k2, b->k2) + qtZipSumAdd(a->k3, b->k3);
}
static W qtZipSumXor(qt *a, qt *b) {
    if (a == NULL || b == NULL) return 0u;
    return (a->v ^ b->v) + qtZipSumXor(a->k0, b->k0) + qtZipSumXor(a->k1, b->k1)
                         + qtZipSumXor(a->k2, b->k2) + qtZipSumXor(a->k3, b->k3);
}
static int bench(void) {
    W acc = 0xC0FFEEu; int i, j;
    for (i = 0; i < 4; i++) {
        W sa = 0xA1F32C97u + (W)i, sb = 0x5EE9D4B2u + (W)i, s;
        qt *ta = NULL, *tb = NULL, *tm; W sl, sr, za, zx;
        { pl *oa = mkOps(INSERTCOUNT, sa), *ob = mkOps(INSERTCOUNT, sb),
             *od = mkOps(4, sa ^ 0xCAFEu);
          for (; oa; oa = oa->tl) ta = qtInsert(ta, oa->a, oa->b);
          for (; ob; ob = ob->tl) tb = qtInsert(tb, ob->a, ob->b);
          for (; od; od = od->tl) ta = qtDelete(ta, od->a); }
        tm = qtMap3p1(ta);
        sl = qtFoldlAdd(0u, tm); sr = qtFoldrXor(0u, tm);
        za = qtZipSumAdd(tm, tb); zx = qtZipSumXor(tm, tb);
        acc = hashMix(hashMix(hashMix(hashMix(acc, sl), sr), za), zx);
    }
    printf("%u\n", acc);
    return 0;
}
int main(void){void*s=bmperf_alloc(),*e=bmperf_alloc();bmperf_snap(s);int r=bench();bmperf_snap(e);bmperf_report(s,e);return r;}
