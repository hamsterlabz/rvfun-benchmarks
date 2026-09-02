extern void *bmperf_alloc(void); extern void bmperf_snap(void *);
extern void bmperf_report(void *, void *);
/* btree, C: the gadt BTreePure benchmark, node-for-node.  A tree is malloc'd
   struct nodes -- NO arrays -- so this is the same shape the Haskell version
   builds, and every operation (insert, delete, map, the two in-order folds and
   the structure-aligned zip-sum) mirrors its Haskell counterpart exactly.
   W is uint32_t because the Haskell W wraps at 32 bits and shiftR there is the
   LOGICAL shift.  Answer 1086800167, the same as the Haskell version. */
#include <stdio.h>
#include <stdlib.h>
typedef unsigned int W;
typedef struct bt { W k; struct bt *l, *r; } bt;

static bt *node(W k, bt *l, bt *r) {
    bt *n = (bt *)malloc(sizeof(bt)); n->k = k; n->l = l; n->r = r; return n;
}
/* The Haskell driver reifies the op stream as a real cons list (`keys` /
   `vals` / `pairs` / `mkOps`) and folds over it, so the port allocates the same
   list.  Generating the ops inline in the loop would hand C a stack-only
   shortcut the benchmark does not have; every structure here is heap data. */
typedef struct wl { W hd; struct wl *tl; } wl;
static wl *wcons(W h, wl *t) { wl *w = (wl *)malloc(sizeof(wl)); w->hd = h; w->tl = t; return w; }
/* keys n s = (s' .&. 4095) : keys (n-1) s' */
static wl *keys(int n, W s) {
    if (n <= 0) return NULL;
    { W s1 = s * 1664525u + 1013904223u; return wcons(s1 & 4095u, keys(n - 1, s1)); }
}
static W lcgNext(W s) { return s * 1664525u + 1013904223u; }
static W hashMix(W h, W v) {
    W a = h ^ v, b = a * 0x85ebca6bu, c = b ^ (b >> 13), d = c * 0xc2b2ae35u;
    return d ^ (d >> 16);
}
static bt *bInsert(bt *t, W k) {
    if (t == NULL) return node(k, NULL, NULL);
    if (k < t->k) return node(t->k, bInsert(t->l, k), t->r);
    if (k > t->k) return node(t->k, t->l, bInsert(t->r, k));
    return t;
}
static W bMinKey(bt *t) {
    if (t == NULL) return 0;
    if (t->l == NULL) return t->k;
    return bMinKey(t->l);
}
static bt *bDeleteMin(bt *t) {
    if (t == NULL) return NULL;
    if (t->l == NULL) return t->r;
    return node(t->k, bDeleteMin(t->l), t->r);
}
static bt *bDelete(bt *t, W k) {
    if (t == NULL) return NULL;
    if (k < t->k) return node(t->k, bDelete(t->l, k), t->r);
    if (k > t->k) return node(t->k, t->l, bDelete(t->r, k));
    if (t->l == NULL) return t->r;
    if (t->r == NULL) return t->l;
    return node(bMinKey(t->r), t->l, bDeleteMin(t->r));
}
static bt *bMap3p1(bt *t) {   /* mul3plus1 */
    if (t == NULL) return NULL;
    return node(t->k * 3u + 1u, bMap3p1(t->l), bMap3p1(t->r));
}
/* bFoldl f z (Node v l r) = bFoldl f (f (bFoldl f z l) v) r   -- f = add */
static W bFoldlAdd(W z, bt *t) {
    if (t == NULL) return z;
    return bFoldlAdd(bFoldlAdd(z, t->l) + t->k, t->r);
}
/* bFoldr f z (Node v l r) = bFoldr f (f v (bFoldr f z r)) l   -- f = xor */
static W bFoldrXor(W z, bt *t) {
    if (t == NULL) return z;
    return bFoldrXor(t->k ^ bFoldrXor(z, t->r), t->l);
}
static W bZipAdd(bt *a, bt *b) {
    if (a == NULL || b == NULL) return 0;
    return (a->k + b->k) + bZipAdd(a->l, b->l) + bZipAdd(a->r, b->r);
}
static W bZipXor(bt *a, bt *b) {
    if (a == NULL || b == NULL) return 0;
    return (a->k ^ b->k) + bZipXor(a->l, b->l) + bZipXor(a->r, b->r);
}
/* keys n s = let s0 = lcgNext s in (s0 & 4095) : keys (n-1) s0 */
/* foldl bInsert / bDelete over the key list */
static bt *insertKeys(bt *t, wl *ks) { for (; ks; ks = ks->tl) t = bInsert(t, ks->hd); return t; }
static bt *deleteKeys(bt *t, wl *ks) { for (; ks; ks = ks->tl) t = bDelete(t, ks->hd); return t; }
#define KEYCOUNT 24
static int bench(void) {
    W acc = 0xC0FFEEu; int i;
    for (i = 0; i < 4; i++) {
        W sa = 0xA1F32C97u + (W)i, sb = 0x5EE9D4B2u + (W)i;
        bt *ta = insertKeys(NULL, keys(KEYCOUNT, sa));
        bt *tb = insertKeys(NULL, keys(KEYCOUNT, sb));
        bt *taD = deleteKeys(ta, keys(4, sa ^ 0xDEADu));
        bt *tm = bMap3p1(taD);
        W sl = bFoldlAdd(0, tm), sr = bFoldrXor(0, tm);
        W za = bZipAdd(tm, tb), zx = bZipXor(tm, tb);
        acc = hashMix(hashMix(hashMix(hashMix(acc, sl), sr), za), zx);
    }
    printf("%u\n", acc);
    return 0;
}
int main(void){void*s=bmperf_alloc(),*e=bmperf_alloc();bmperf_snap(s);int r=bench();bmperf_snap(e);bmperf_report(s,e);return r;}
