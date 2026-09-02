extern void *bmperf_alloc(void); extern void bmperf_snap(void *);
extern void bmperf_report(void *, void *);
/* zipper, C: the gadt ZipperPure benchmark.  A binary tree plus a Huet ZIPPER:
   the trail is a linked list of crumbs, each holding the direction taken, the
   parent's value and the sibling subtree.  Everything is malloc'd nodes and
   cons cells -- NO arrays, including tToList, which is a real list.
   W is uint32_t (the Haskell W wraps at 32 bits).  Answer 739504078. */
#include <stdio.h>
#include <stdlib.h>
typedef unsigned int W;
typedef struct tree { W v; struct tree *l, *r; } tree;          /* NULL = Leaf */
enum { DL = 0, DR = 1 };
typedef struct crumb { int dir; W v; tree *sib; struct crumb *tl; } crumb;
typedef struct { tree *foc; crumb *ts; } zip;
typedef struct wl { W hd; struct wl *tl; } wl;                  /* a list of W */

static tree *node(W v, tree *l, tree *r) {
    tree *t = (tree *)malloc(sizeof(tree)); t->v = v; t->l = l; t->r = r; return t;
}
static crumb *push(int dir, W v, tree *sib, crumb *ts) {
    crumb *c = (crumb *)malloc(sizeof(crumb)); c->dir = dir; c->v = v; c->sib = sib; c->tl = ts; return c;
}
/* keys n s = (s' .&. 0xFFFF) : keys (n-1) s' -- the Haskell driver builds this
   list and folds tInsert over it, so the port allocates it too. */
static wl *keys(int n, W s);
static W lcgNext(W s) { return s * 1664525u + 1013904223u; }
static wl *wcons(W h, wl *t);
static wl *keys(int n, W s) {
    if (n <= 0) return NULL;
    { W s1 = lcgNext(s); return wcons(s1 & 0xFFFFu, keys(n - 1, s1)); }
}
static W hashMix(W h, W v) {
    W a = h ^ v, b = a * 0x85ebca6bu, c = b ^ (b >> 13), d = c * 0xc2b2ae35u;
    return d ^ (d >> 16);
}
static zip zDownL(zip z) { zip o = z; if (z.foc == NULL) return z;
    o.foc = z.foc->l; o.ts = push(DL, z.foc->v, z.foc->r, z.ts); return o; }
static zip zDownR(zip z) { zip o = z; if (z.foc == NULL) return z;
    o.foc = z.foc->r; o.ts = push(DR, z.foc->v, z.foc->l, z.ts); return o; }
static zip zUp(zip z) {
    zip o; if (z.ts == NULL) return z;
    o.ts = z.ts->tl;
    o.foc = (z.ts->dir == DL) ? node(z.ts->v, z.foc, z.ts->sib)
                              : node(z.ts->v, z.ts->sib, z.foc);
    return o;
}
static zip zTop(zip z) { while (z.ts != NULL) z = zUp(z); return z; }
static zip zModify3p1(zip z) { zip o = z; if (z.foc == NULL) return z;
    o.foc = node(z.foc->v * 3u + 1u, z.foc->l, z.foc->r); return o; }
static zip zReplace(tree *t, zip z) { zip o; o.foc = t; o.ts = z.ts; return o; }
static tree *tInsert(tree *t, W v) {
    if (t == NULL) return node(v, NULL, NULL);
    if (v == t->v) return t;
    if (v < t->v)  return node(t->v, tInsert(t->l, v), t->r);
    return node(t->v, t->l, tInsert(t->r, v));
}
static W tFoldlAdd(W z, tree *t) {
    if (t == NULL) return z;
    return tFoldlAdd(tFoldlAdd(z, t->l) + t->v, t->r);
}
static W tFoldrXor(W z, tree *t) {
    if (t == NULL) return z;
    return tFoldrXor(t->v ^ tFoldrXor(z, t->r), t->l);
}
static tree *tMap3p1(tree *t) {
    if (t == NULL) return NULL;
    return node(t->v * 3u + 1u, tMap3p1(t->l), tMap3p1(t->r));
}
/* tToList: in-order, as a real cons list.  Built by appending onto a tail so
   the traversal stays the Haskell's  tToList l ++ [v] ++ tToList r. */
static wl *wcons(W h, wl *t) { wl *w = (wl *)malloc(sizeof(wl)); w->hd = h; w->tl = t; return w; }
static wl *tToList(tree *t, wl *tail) {
    if (t == NULL) return tail;
    return tToList(t->l, wcons(t->v, tToList(t->r, tail)));
}
static W zipSumAdd(wl *a, wl *b) { W s = 0; for (; a && b; a = a->tl, b = b->tl) s += a->hd + b->hd; return s; }
static W zipSumXor(wl *a, wl *b) { W s = 0; for (; a && b; a = a->tl, b = b->tl) s += a->hd ^ b->hd; return s; }
#define TLEN 16
static int bench(void) {
    W acc = 0xC0FFEEu; int i, j;
    for (i = 0; i < 4; i++) {
        W sa = 0xA1F32C97u + (W)i, sb = 0x5EE9D4B2u + (W)i, s;
        tree *ta = NULL, *tb = NULL, *tm; zip z; W sl, sr, za, zx;
        { wl *ka = keys(TLEN, sa), *kb = keys(TLEN, sb);
          for (; ka; ka = ka->tl) ta = tInsert(ta, ka->hd);
          for (; kb; kb = kb->tl) tb = tInsert(tb, kb->hd); }
        z.foc = ta; z.ts = NULL;
        z = zModify3p1(zDownR(zDownL(z)));               /* zp1 */
        z = zUp(zUp(zReplace(node(0xCAFEBABEu, NULL, NULL), z)));  /* zp2 */
        z = zReplace(NULL, zDownL(z));                    /* zp3: zDelete */
        z = zTop(z);
        tm = tMap3p1(z.foc);
        sl = tFoldlAdd(0, tm); sr = tFoldrXor(0, tm);
        za = zipSumAdd(tToList(tm, NULL), tToList(tb, NULL));
        zx = zipSumXor(tToList(tm, NULL), tToList(tb, NULL));
        acc = hashMix(hashMix(hashMix(hashMix(acc, sl), sr), za), zx);
    }
    printf("%u\n", acc);
    return 0;
}
int main(void){void*s=bmperf_alloc(),*e=bmperf_alloc();bmperf_snap(s);int r=bench();bmperf_snap(e);bmperf_report(s,e);return r;}
