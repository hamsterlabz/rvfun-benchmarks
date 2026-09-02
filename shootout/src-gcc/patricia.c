extern void *bmperf_alloc(void); extern void bmperf_snap(void *);
extern void bmperf_report(void *, void *);
/* patricia, C: the gadt PatriciaPure benchmark, constructor for constructor.
   A big-endian Patricia trie of malloc'd nodes -- NO arrays.  The Haskell type
   has three constructors (Empty | Leaf k v | Branch m p l r), so the C node
   carries a tag.  W is uint32_t: the Haskell W wraps at 32 bits and its shiftR
   is the LOGICAL shift.  Answer 889214830, the same as the Haskell version. */
#include <stdio.h>
#include <stdlib.h>
typedef unsigned int W;
enum { EMPTY = 0, LEAF = 1, BRANCH = 2 };
typedef struct pt { int tag; W a, b; struct pt *l, *r; } pt;   /* Leaf: a=k b=v ; Branch: a=m b=p */

static pt *leaf(W k, W v) {
    pt *n = (pt *)malloc(sizeof(pt)); n->tag = LEAF; n->a = k; n->b = v; n->l = n->r = NULL; return n;
}
static pt *branch(W m, W p, pt *l, pt *r) {
    pt *n = (pt *)malloc(sizeof(pt)); n->tag = BRANCH; n->a = m; n->b = p; n->l = l; n->r = r; return n;
}
typedef struct wl { W hd; struct wl *tl; } wl;
static wl *wcons(W h, wl *t) { wl *w = (wl *)malloc(sizeof(wl)); w->hd = h; w->tl = t; return w; }
typedef struct pl { W a, b; struct pl *tl; } pl;
static pl *pcons(W a, W b, pl *t) { pl *p = (pl *)malloc(sizeof(pl)); p->a = a; p->b = b; p->tl = t; return p; }
/* pairs n s = (s1, s2) : pairs (n-1) s2 */
static pl *pairs(int n, W s) {
    if (n <= 0) return NULL;
    { W s1 = s * 1664525u + 1013904223u, s2 = s1 * 1664525u + 1013904223u;
      return pcons(s1, s2, pairs(n - 1, s2)); }
}
/* map fst (take 4 ks) -- its own list, as in the Haskell */
static wl *wcons(W h, wl *t);
static wl *mapFstTake4(pl *ks) {
    int n = 4; wl *out = NULL, **tail = &out;
    for (; ks && n > 0; ks = ks->tl, n--) { *tail = wcons(ks->a, NULL); tail = &(*tail)->tl; }
    return out;
}
static W lcgNext(W s) { return s * 1664525u + 1013904223u; }
static W hashMix(W h, W v) {
    W a = h ^ v, b = a * 0x85ebca6bu, c = b ^ (b >> 13), d = c * 0xc2b2ae35u;
    return d ^ (d >> 16);
}
static W highestBit(W x) {
    x |= x >> 1; x |= x >> 2; x |= x >> 4; x |= x >> 8; x |= x >> 16;
    return (x + 1u) >> 1;
}
static W branchingBit(W p0, W p1) { return highestBit(p0 ^ p1); }
static W maskOf(W k, W m) { return k & (m - 1u); }
static int zeroBit(W k, W m) { return (k & m) == 0; }
static int matchPrefix(W k, W p, W m) { return maskOf(k, m) == p; }

static pt *ptInsert(pt *t, W k, W v) {
    if (t == NULL) return leaf(k, v);
    if (t->tag == LEAF) {
        if (t->a == k) return leaf(k, v);
        { W m = branchingBit(t->a, k); pt *nl = leaf(k, v);
          return zeroBit(k, m) ? branch(m, maskOf(k, m), nl, t)
                               : branch(m, maskOf(k, m), t, nl); }
    }
    if (!matchPrefix(k, t->b, t->a)) {
        W m2 = branchingBit(k, t->b); pt *nl = leaf(k, v);
        return zeroBit(k, m2) ? branch(m2, maskOf(k, m2), nl, t)
                              : branch(m2, maskOf(k, m2), t, nl);
    }
    if (zeroBit(k, t->a)) return branch(t->a, t->b, ptInsert(t->l, k, v), t->r);
    return branch(t->a, t->b, t->l, ptInsert(t->r, k, v));
}
static pt *ptDelete(pt *t, W k) {
    if (t == NULL) return NULL;
    if (t->tag == LEAF) return (t->a == k) ? NULL : t;
    if (!matchPrefix(k, t->b, t->a)) return t;
    if (zeroBit(k, t->a)) { pt *nl = ptDelete(t->l, k);
        return nl == NULL ? t->r : branch(t->a, t->b, nl, t->r); }
    { pt *nr = ptDelete(t->r, k);
      return nr == NULL ? t->l : branch(t->a, t->b, t->l, nr); }
}
/* ptLookup: 1 = found, *out set */
static int ptLookup(pt *t, W k, W *out) {
    if (t == NULL) return 0;
    if (t->tag == LEAF) { if (t->a == k) { *out = t->b; return 1; } return 0; }
    return ptLookup(zeroBit(k, t->a) ? t->l : t->r, k, out);
}
static W ptFoldlAdd(W z, pt *t) {
    if (t == NULL) return z;
    if (t->tag == LEAF) return z + t->b;
    return ptFoldlAdd(ptFoldlAdd(z, t->l), t->r);
}
static W ptFoldrXor(W z, pt *t) {
    if (t == NULL) return z;
    if (t->tag == LEAF) return t->b ^ z;
    return ptFoldrXor(ptFoldrXor(z, t->r), t->l);
}
static pt *ptMap3p1(pt *t) {
    if (t == NULL) return NULL;
    if (t->tag == LEAF) return leaf(t->a, t->b * 3u + 1u);
    return branch(t->a, t->b, ptMap3p1(t->l), ptMap3p1(t->r));
}
static W ptZipAdd(pt *a, pt *b) {
    W v;
    if (a == NULL) return 0;
    if (a->tag == LEAF) return ptLookup(b, a->a, &v) ? (a->b + v) : 0;
    return ptZipAdd(a->l, b) + ptZipAdd(a->r, b);
}
static W ptZipXor(pt *a, pt *b) {
    W v;
    if (a == NULL) return 0;
    if (a->tag == LEAF) return ptLookup(b, a->a, &v) ? (a->b ^ v) : 0;
    return ptZipXor(a->l, b) + ptZipXor(a->r, b);
}
#define KEYCOUNT 16
static int bench(void) {
    W acc = 0xC0FFEEu; int i, j;
    for (i = 0; i < 4; i++) {
        W sa = 0xA1F32C97u + (W)i, sb = 0x5EE9D4B2u + (W)i;
        pt *ta = NULL, *tb = NULL, *tm; W sl, sr, za, zx;
        pl *ksA = pairs(KEYCOUNT, sa), *ksB = pairs(KEYCOUNT, sb);
        wl *dks = mapFstTake4(ksA);
        { pl *q; wl *w;
          for (q = ksA; q; q = q->tl) ta = ptInsert(ta, q->a, q->b);
          for (q = ksB; q; q = q->tl) tb = ptInsert(tb, q->a, q->b);
          for (w = dks; w; w = w->tl) ta = ptDelete(ta, w->hd); }
        tm = ptMap3p1(ta);
        sl = ptFoldlAdd(0, tm); sr = ptFoldrXor(0, tm);
        za = ptZipAdd(tm, tb);  zx = ptZipXor(tm, tb);
        acc = hashMix(hashMix(hashMix(hashMix(acc, sl), sr), za), zx);
    }
    printf("%u\n", acc);
    return 0;
}
int main(void){void*s=bmperf_alloc(),*e=bmperf_alloc();bmperf_snap(s);int r=bench();bmperf_snap(e);bmperf_report(s,e);return r;}
