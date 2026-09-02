extern void *bmperf_alloc(void); extern void bmperf_snap(void *);
extern void bmperf_report(void *, void *);
/* rosetree, C: the gadt RoseTreePure benchmark.  `data Rose = Rose W [Rose]`
   becomes a malloc'd node holding a value and a linked LIST of children -- NO
   arrays, since the Haskell child list is a cons list and the folds walk it.
   W is uint32_t (the Haskell W wraps at 32 bits).  Answer 2111870790. */
#include <stdio.h>
#include <stdlib.h>
typedef unsigned int W;
struct rose;
typedef struct kids { struct rose *hd; struct kids *tl; } kids;
typedef struct rose { W v; kids *ks; } rose;

static rose *mkRose(W v, kids *ks) {
    rose *r = (rose *)malloc(sizeof(rose)); r->v = v; r->ks = ks; return r;
}
static kids *cons(rose *h, kids *t) {
    kids *k = (kids *)malloc(sizeof(kids)); k->hd = h; k->tl = t; return k;
}
/* The Haskell driver reifies the op stream as a real cons list (`keys` /
   `vals` / `pairs` / `mkOps`) and folds over it, so the port allocates the same
   list.  Generating the ops inline in the loop would hand C a stack-only
   shortcut the benchmark does not have; every structure here is heap data. */
typedef struct wl { W hd; struct wl *tl; } wl;
static wl *wcons(W h, wl *t) { wl *w = (wl *)malloc(sizeof(wl)); w->hd = h; w->tl = t; return w; }
/* vals n s = s' : vals (n-1) s' */
static wl *vals(int n, W s) {
    if (n <= 0) return NULL;
    { W s1 = s * 1664525u + 1013904223u; return wcons(s1, vals(n - 1, s1)); }
}
static W lcgNext(W s) { return s * 1664525u + 1013904223u; }
static W hashMix(W h, W v) {
    W a = h ^ v, b = a * 0x85ebca6bu, c = b ^ (b >> 13), d = c * 0xc2b2ae35u;
    return d ^ (d >> 16);
}
/* Rose v ks  ->  Rose v (Rose v' [] : ks)   -- new child at the FRONT */
static rose *insertChild(rose *t, W v2) { return mkRose(t->v, cons(mkRose(v2, NULL), t->ks)); }
static rose *deleteFirstChild(rose *t) { return t->ks == NULL ? t : mkRose(t->v, t->ks->tl); }
/* roseFoldl f z (Rose v ks) = foldl (roseFoldl f) (f z v) ks */
static W roseFoldlAdd(W z, rose *t) {
    kids *k; W acc = z + t->v;
    for (k = t->ks; k != NULL; k = k->tl) acc = roseFoldlAdd(acc, k->hd);
    return acc;
}
/* roseFoldr f z (Rose v ks) = f v (foldr (flip (roseFoldr f)) z ks)
   foldr over the child list means the LAST child folds first, which is plain
   recursion down the list and application on the way back -- no temporary
   buffer, so the structure stays array-free the way the Haskell is. */
static W roseFoldrXor(W z, rose *t);
static W foldrKids(kids *k, W z) {
    if (k == NULL) return z;
    return roseFoldrXor(foldrKids(k->tl, z), k->hd);
}
static W roseFoldrXor(W z, rose *t) { return t->v ^ foldrKids(t->ks, z); }
static rose *roseMap3p1(rose *t) {
    kids *k, *head = NULL, **tail = &head;
    for (k = t->ks; k != NULL; k = k->tl) { *tail = cons(roseMap3p1(k->hd), NULL); tail = &(*tail)->tl; }
    return mkRose(t->v * 3u + 1u, head);
}
static W roseZipAdd(rose *a, rose *b) {
    kids *ka, *kb; W s = a->v + b->v;
    for (ka = a->ks, kb = b->ks; ka != NULL && kb != NULL; ka = ka->tl, kb = kb->tl)
        s += roseZipAdd(ka->hd, kb->hd);
    return s;
}
static W roseZipXor(rose *a, rose *b) {
    kids *ka, *kb; W s = a->v ^ b->v;
    for (ka = a->ks, kb = b->ks; ka != NULL && kb != NULL; ka = ka->tl, kb = kb->tl)
        s += roseZipXor(ka->hd, kb->hd);
    return s;
}
#define INSERTS 16
static int bench(void) {
    W acc = 0xC0FFEEu; int i, j;
    for (i = 0; i < 4; i++) {
        W sa = 0xA1F32C97u + (W)i, sb = 0x5EE9D4B2u + (W)i, s;
        rose *ta = mkRose(lcgNext(sa), NULL), *tb = mkRose(lcgNext(sb), NULL), *tm;
        W sl, sr, za, zx;
        { wl *va = vals(INSERTS, sa), *vb = vals(INSERTS, sb);
          for (; va; va = va->tl) ta = insertChild(ta, va->hd);
          for (; vb; vb = vb->tl) tb = insertChild(tb, vb->hd); }
        ta = deleteFirstChild(deleteFirstChild(ta));
        tm = roseMap3p1(ta);
        sl = roseFoldlAdd(0, tm); sr = roseFoldrXor(0, tm);
        za = roseZipAdd(tm, tb);  zx = roseZipXor(tm, tb);
        acc = hashMix(hashMix(hashMix(hashMix(acc, sl), sr), za), zx);
    }
    printf("%u\n", acc);
    return 0;
}
int main(void){void*s=bmperf_alloc(),*e=bmperf_alloc();bmperf_snap(s);int r=bench();bmperf_snap(e);bmperf_report(s,e);return r;}
