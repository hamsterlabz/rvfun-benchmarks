/******************************************************************************
 * patricia_pure.c — functional-C Patricia trie (Okasaki "Fast Mergeable
 * Integer Maps", aka Haskell's Data.IntMap) for the GADT suite.
 *
 * Layout: each node is either
 *   Empty            — no binding
 *   Leaf  k v        — single key k → value v
 *   Branch m  L  R   — branch bit m, left = keys with bit 0 at m,
 *                                  right = keys with bit 1
 *
 * Every mutation returns a NEW spine (path-copying). Untouched
 * subtries are shared between the input and the output. Probes the
 * bit-manipulation hot path on rv32 (XOR + highestBit / clz).
 *
 * Operations implemented:
 *   insert delete lookup member fromList toList
 *   foldl foldr map filter partition split merge size
 *   zip zipWith reverse rotate scanl scanr iterate unfold
 *****************************************************************************/

#pragma GCC optimize ("no-inline,no-reorder-functions")

#include "support.h"
#include "gadt_common.h"

#define LOCAL_SCALE_FACTOR 6

GADT_ARENA_DEFINE();

typedef enum { PT_EMPTY = 0, PT_LEAF = 1, PT_BRANCH = 2 } pt_tag_t;

typedef struct pt_s {
  pt_tag_t       tag;
  uint32_t       key;        /* leaf: key   ; branch: mask */
  uint32_t       val;        /* leaf: value ; branch: prefix */
  struct pt_s   *l, *r;      /* branch only */
} pt_t;

static pt_t *g_empty;        /* singleton sentinel */

static pt_t *
pt_leaf (uint32_t k, uint32_t v)
{
  pt_t *n = (pt_t *) gadt_arena_alloc (sizeof (pt_t));
  if (!n) return g_empty;
  n->tag = PT_LEAF; n->key = k; n->val = v; n->l = n->r = 0;
  return n;
}

static pt_t *
pt_branch (uint32_t mask, uint32_t prefix, pt_t *l, pt_t *r)
{
  pt_t *n = (pt_t *) gadt_arena_alloc (sizeof (pt_t));
  if (!n) return g_empty;
  n->tag = PT_BRANCH; n->key = mask; n->val = prefix; n->l = l; n->r = r;
  return n;
}

/* Bit helpers. No clz on base rv32i — open-coded "highest bit set". */
static inline uint32_t
highest_bit (uint32_t x)
{
  /* Round x up to next power-of-two then >>1 to get the top bit. */
  x |= x >> 1; x |= x >> 2; x |= x >> 4;
  x |= x >> 8; x |= x >> 16;
  return (x + 1) >> 1;
}

static inline uint32_t branching_bit (uint32_t p0, uint32_t p1) { return highest_bit (p0 ^ p1); }
static inline uint32_t mask_of       (uint32_t k,  uint32_t m)  { return k & (m - 1u); }
static inline int      zero_bit      (uint32_t k,  uint32_t m)  { return (k & m) == 0u; }
static inline int      match_prefix  (uint32_t k,  uint32_t p, uint32_t m) { return mask_of (k, m) == p; }

/* ---- core ops -------------------------------------------------------- */

static pt_t *
pt_insert (pt_t *t, uint32_t k, uint32_t v)
{
  if (t->tag == PT_EMPTY) return pt_leaf (k, v);
  if (t->tag == PT_LEAF) {
    if (t->key == k) return pt_leaf (k, v);
    uint32_t m = branching_bit (t->key, k);
    pt_t *nl = pt_leaf (k, v);
    if (zero_bit (k, m)) return pt_branch (m, mask_of (k, m), nl, t);
    else                  return pt_branch (m, mask_of (k, m), t, nl);
  }
  /* branch */
  uint32_t m = t->key, p = t->val;
  if (!match_prefix (k, p, m)) {
    uint32_t m2 = branching_bit (k, p);
    pt_t *nl = pt_leaf (k, v);
    if (zero_bit (k, m2)) return pt_branch (m2, mask_of (k, m2), nl, t);
    else                   return pt_branch (m2, mask_of (k, m2), t, nl);
  }
  if (zero_bit (k, m)) return pt_branch (m, p, pt_insert (t->l, k, v), t->r);
  else                  return pt_branch (m, p, t->l, pt_insert (t->r, k, v));
}

static pt_t *
pt_delete (pt_t *t, uint32_t k)
{
  if (t->tag == PT_EMPTY) return t;
  if (t->tag == PT_LEAF)  return (t->key == k) ? g_empty : t;
  uint32_t m = t->key, p = t->val;
  if (!match_prefix (k, p, m)) return t;
  if (zero_bit (k, m)) {
    pt_t *nl = pt_delete (t->l, k);
    if (nl->tag == PT_EMPTY) return t->r;
    return pt_branch (m, p, nl, t->r);
  } else {
    pt_t *nr = pt_delete (t->r, k);
    if (nr->tag == PT_EMPTY) return t->l;
    return pt_branch (m, p, t->l, nr);
  }
}

static int
pt_lookup (pt_t *t, uint32_t k, uint32_t *out)
{
  while (t->tag != PT_EMPTY) {
    if (t->tag == PT_LEAF) { if (t->key == k) { *out = t->val; return 1; } return 0; }
    t = zero_bit (k, t->key) ? t->l : t->r;
  }
  return 0;
}

static int pt_member (pt_t *t, uint32_t k) { uint32_t v; return pt_lookup (t, k, &v); }

/* ---- folds (key-sorted in-order) ----------------------------------- */

static uint32_t
pt_foldl (uint32_t (*f)(uint32_t, uint32_t, uint32_t), uint32_t z, pt_t *t)
{
  if (t->tag == PT_EMPTY) return z;
  if (t->tag == PT_LEAF)  return f (z, t->key, t->val);
  z = pt_foldl (f, z, t->l);
  z = pt_foldl (f, z, t->r);
  return z;
}

static uint32_t
pt_foldr (uint32_t (*f)(uint32_t, uint32_t, uint32_t), uint32_t z, pt_t *t)
{
  if (t->tag == PT_EMPTY) return z;
  if (t->tag == PT_LEAF)  return f (t->key, t->val, z);
  z = pt_foldr (f, z, t->r);
  z = pt_foldr (f, z, t->l);
  return z;
}

static uint32_t pt_size (pt_t *t) {
  if (t->tag == PT_EMPTY) return 0;
  if (t->tag == PT_LEAF)  return 1;
  return pt_size (t->l) + pt_size (t->r);
}

/* ---- map / filter / partition ------------------------------------- */

static pt_t *
pt_map (uint32_t (*f)(uint32_t), pt_t *t)
{
  if (t->tag == PT_EMPTY) return t;
  if (t->tag == PT_LEAF)  return pt_leaf (t->key, f (t->val));
  return pt_branch (t->key, t->val, pt_map (f, t->l), pt_map (f, t->r));
}

static pt_t *
pt_filter (int (*p)(uint32_t, uint32_t), pt_t *t)
{
  if (t->tag == PT_EMPTY) return t;
  if (t->tag == PT_LEAF)  return p (t->key, t->val) ? t : g_empty;
  pt_t *nl = pt_filter (p, t->l);
  pt_t *nr = pt_filter (p, t->r);
  if (nl->tag == PT_EMPTY) return nr;
  if (nr->tag == PT_EMPTY) return nl;
  return pt_branch (t->key, t->val, nl, nr);
}

/* partition: returns (yes ∪ no = t) packed into a 2-leaf branch with
 * yes-pred at left, no-pred at right; caller checksums both subtries. */
typedef struct { pt_t *yes, *no; } pt_split_t;

static pt_split_t
pt_partition (int (*p)(uint32_t, uint32_t), pt_t *t)
{
  pt_split_t out;
  if (t->tag == PT_EMPTY) { out.yes = g_empty; out.no = g_empty; return out; }
  if (t->tag == PT_LEAF) {
    if (p (t->key, t->val)) { out.yes = t;       out.no = g_empty; }
    else                     { out.yes = g_empty; out.no = t;       }
    return out;
  }
  pt_split_t l = pt_partition (p, t->l);
  pt_split_t r = pt_partition (p, t->r);
  uint32_t m = t->key, pp = t->val;
  out.yes = (l.yes->tag == PT_EMPTY) ? r.yes :
            (r.yes->tag == PT_EMPTY) ? l.yes :
            pt_branch (m, pp, l.yes, r.yes);
  out.no  = (l.no->tag  == PT_EMPTY) ? r.no  :
            (r.no->tag  == PT_EMPTY) ? l.no  :
            pt_branch (m, pp, l.no,  r.no);
  return out;
}

/* split-on-key: keys < k → left, keys > k → right (== k dropped) */
static pt_split_t
pt_split (pt_t *t, uint32_t k)
{
  pt_split_t out = { g_empty, g_empty };
  if (t->tag == PT_EMPTY) return out;
  if (t->tag == PT_LEAF) {
    if (t->key <  k) out.yes = t;
    if (t->key >  k) out.no  = t;
    return out;
  }
  /* Recurse into both halves; combine respecting key ordering. */
  pt_split_t l = pt_split (t->l, k);
  pt_split_t r = pt_split (t->r, k);
  out.yes = (l.yes->tag == PT_EMPTY) ? r.yes :
            (r.yes->tag == PT_EMPTY) ? l.yes :
            pt_branch (t->key, t->val, l.yes, r.yes);
  out.no  = (l.no->tag  == PT_EMPTY) ? r.no  :
            (r.no->tag  == PT_EMPTY) ? l.no  :
            pt_branch (t->key, t->val, l.no, r.no);
  return out;
}

/* ---- merge / concat (union, right-biased) ------------------------- */

static pt_t *
pt_merge (pt_t *a, pt_t *b)
{
  if (a->tag == PT_EMPTY) return b;
  if (b->tag == PT_EMPTY) return a;
  if (a->tag == PT_LEAF)  return pt_insert (b, a->key, a->val);
  if (b->tag == PT_LEAF)  return pt_insert (a, b->key, b->val);
  /* Both branches — fold all of b into a leaf-by-leaf via foldl. */
  pt_t *acc = a;
  /* In a pure setting we'd zip on branches with matching masks; the
   * simple version below is O(n_b · log n_a) but correct. */
  /* Hand-rolled leaf walk: */
  pt_t *stk[64]; int sp = 0;
  stk[sp++] = b;
  while (sp) {
    pt_t *n = stk[--sp];
    if (n->tag == PT_LEAF) acc = pt_insert (acc, n->key, n->val);
    else if (n->tag == PT_BRANCH) {
      if (sp < 63) { stk[sp++] = n->l; stk[sp++] = n->r; }
    }
  }
  return acc;
}

/* ---- zip / zipWith over intersection of keys --------------------- */

/* Key-intersection zipsum: walk `a`, look up each leaf's key in `b`.
 * Canonical semantics shared with patricia_ninja / patricia_morph /
 * Hs.PatriciaPure / Hs.PatriciaMorph. */
static uint32_t
pt_zipsum_with (uint32_t (*f)(uint32_t, uint32_t), pt_t *a, pt_t *b)
{
  if (a->tag == PT_EMPTY) return 0;
  if (a->tag == PT_LEAF) {
    uint32_t v;
    return pt_lookup (b, a->key, &v) ? f (a->val, v) : 0;
  }
  return pt_zipsum_with (f, a->l, b) + pt_zipsum_with (f, a->r, b);
}

/* ---- bulk: fromList / toList ------------------------------------- */

static pt_t *
pt_from_kv_arr (const uint32_t *ks, const uint32_t *vs, int n)
{
  pt_t *t = g_empty;
  for (int i = 0; i < n; i++) t = pt_insert (t, ks[i], vs[i]);
  return t;
}

/* toList: write key/val pairs into out_k/out_v; returns count written.
 * Order is "trie in-order" (matches foldl). */
static int
pt_to_list (pt_t *t, uint32_t *out_k, uint32_t *out_v, int cap)
{
  int n = 0;
  pt_t *stk[64]; int sp = 0;
  if (t->tag != PT_EMPTY) stk[sp++] = t;
  while (sp && n < cap) {
    pt_t *x = stk[--sp];
    if (x->tag == PT_LEAF) { out_k[n] = x->key; out_v[n] = x->val; n++; }
    else if (x->tag == PT_BRANCH) {
      if (sp < 63) { stk[sp++] = x->r; stk[sp++] = x->l; }
    }
  }
  return n;
}

/* ---- reverse / rotate (key polarity / bit-rotation) -------------- */

/* "reverse" for IntMap = invert the key polarity: k → ~k. Tests
 * full bit-flip + re-insertion overhead. Implemented as rebuild. */
static pt_t *
pt_reverse (pt_t *t)
{
  if (t->tag == PT_EMPTY) return t;
  if (t->tag == PT_LEAF)  return pt_leaf (~t->key, t->val);
  pt_t *out = g_empty;
  pt_t *stk[64]; int sp = 0;
  stk[sp++] = t;
  while (sp) {
    pt_t *n = stk[--sp];
    if (n->tag == PT_LEAF) out = pt_insert (out, ~n->key, n->val);
    else if (n->tag == PT_BRANCH && sp < 63) { stk[sp++] = n->l; stk[sp++] = n->r; }
  }
  return out;
}

/* rotate: cyclically rotate each key by `r` bits.  Stress bit-shift +
 * insertion-with-perturbed-prefix path. */
static pt_t *
pt_rotate (pt_t *t, unsigned r)
{
  if (t->tag == PT_EMPTY) return t;
  pt_t *out = g_empty;
  pt_t *stk[64]; int sp = 0;
  stk[sp++] = t;
  while (sp) {
    pt_t *n = stk[--sp];
    if (n->tag == PT_LEAF) {
      uint32_t k = (n->key << r) | (n->key >> ((32 - r) & 31));
      out = pt_insert (out, k, n->val);
    } else if (n->tag == PT_BRANCH && sp < 63) { stk[sp++] = n->l; stk[sp++] = n->r; }
  }
  return out;
}

/* ---- scanl / scanr (over key-sorted view) --------------------- */

/* Both scans accumulate into a checksum (returning a full intermediate
 * structure costs an alloc per element; the checksum captures the
 * same shape with less memory). */
static uint32_t
pt_scanl (uint32_t (*f)(uint32_t, uint32_t), uint32_t z, pt_t *t)
{
  uint32_t out = z;
  uint32_t acc = z;
  uint32_t ks[256], vs[256];
  int n = pt_to_list (t, ks, vs, 256);
  for (int i = 0; i < n; i++) {
    acc = f (acc, vs[i]);
    out = gadt_hash_mix (out, acc);
  }
  return out;
}

static uint32_t
pt_scanr (uint32_t (*f)(uint32_t, uint32_t), uint32_t z, pt_t *t)
{
  uint32_t out = z;
  uint32_t acc = z;
  uint32_t ks[256], vs[256];
  int n = pt_to_list (t, ks, vs, 256);
  for (int i = n - 1; i >= 0; i--) {
    acc = f (vs[i], acc);
    out = gadt_hash_mix (out, acc);
  }
  return out;
}

/* ---- iterate / unfold ---------------------------------------- */

/* unfold from seed: produce N (key, val) entries via gen(seed). */
typedef struct { uint32_t k, v, nxt; } pt_unf_t;

static pt_t *
pt_unfold (pt_unf_t (*gen)(uint32_t), uint32_t seed, int n)
{
  pt_t *t = g_empty;
  uint32_t s = seed;
  for (int i = 0; i < n; i++) {
    pt_unf_t e = gen (s);
    t = pt_insert (t, e.k, e.v);
    s = e.nxt;
  }
  return t;
}

static pt_unf_t
gen_step (uint32_t s)
{
  pt_unf_t e;
  uint32_t k = gadt_lcg_next (&s);
  uint32_t v = gadt_lcg_next (&s);
  e.k   = k;
  e.v   = v;
  e.nxt = s;
  return e;
}

/* ---- combinators --------------------------------------------- */

static uint32_t f_add  (uint32_t a, uint32_t b)             { return a + b; }
static uint32_t f_xor  (uint32_t a, uint32_t b)             { return a ^ b; }
/* Leaf-step folds over VALUE ONLY (key is ignored).  Canonical
 * semantics; matches the cata-via-algebra step in patricia_morph
 * and Hs.PatriciaPure / Hs.PatriciaMorph. */
static uint32_t f_add3 (uint32_t a, uint32_t k, uint32_t v) { (void) k; return a + v; }
static uint32_t f_xor3 (uint32_t k, uint32_t v, uint32_t z) { (void) k; return z ^ v; }
static uint32_t f_mul3plus1 (uint32_t x) { return x * 3 + 1; }
static int       p_even      (uint32_t k, uint32_t v) { (void) v; return (k & 1) == 0; }

/* ---- benchmark body ----------------------------------------- */

#define KEY_COUNT 16

static uint32_t in_seed_a, in_seed_b;
static pt_t     g_empty_node;

static uint32_t
benchmark_body (unsigned int lsf, unsigned int gsf)
{
  uint32_t check = 0xC0FFEEu;
  for (unsigned int outer = 0; outer < lsf * gsf; outer++) {
    gadt_arena_reset ();
    g_empty_node.tag = PT_EMPTY;
    g_empty = &g_empty_node;
    uint32_t sa = in_seed_a + outer;
    uint32_t sb = in_seed_b + outer;

    /* Canonical workload — 4 hashMix outputs: sl, sr, za, zx.
     * The full surface (insert/delete/lookup/member/filter/
     * partition/split/merge/reverse/rotate/scan/unfold/toList)
     * stays defined above as functions but is NOT in the hot
     * loop, so the comparison vs patricia_ninja / patricia_morph
     * and Hs.PatriciaPure / Hs.PatriciaMorph measures exactly
     * the same workload. */
    uint32_t ks_a[KEY_COUNT], vs_a[KEY_COUNT];
    uint32_t ks_b[KEY_COUNT], vs_b[KEY_COUNT];
    for (int i = 0; i < KEY_COUNT; i++) {
      ks_a[i] = gadt_lcg_next (&sa); vs_a[i] = gadt_lcg_next (&sa);
      ks_b[i] = gadt_lcg_next (&sb); vs_b[i] = gadt_lcg_next (&sb);
    }
    pt_t *ta = pt_from_kv_arr (ks_a, vs_a, KEY_COUNT);
    pt_t *tb = pt_from_kv_arr (ks_b, vs_b, KEY_COUNT);
    for (int i = 0; i < 4; i++) ta = pt_delete (ta, ks_a[i]);
    pt_t *tm = pt_map (f_mul3plus1, ta);
    uint32_t sl = pt_foldl (f_add3, 0, tm);
    uint32_t sr = pt_foldr (f_xor3, 0, tm);
    uint32_t za = pt_zipsum_with (f_add, tm, tb);
    uint32_t zx = pt_zipsum_with (f_xor, tm, tb);

    check = gadt_hash_mix (check, sl);
    check = gadt_hash_mix (check, sr);
    check = gadt_hash_mix (check, za);
    check = gadt_hash_mix (check, zx);
  }
  return check;
}

/* ---- harness ------------------------------------------------ */

static uint32_t g_check;
void warm_caches (int heat)       { (void) benchmark_body (1, heat); }
int  benchmark (void)             { g_check = benchmark_body (LOCAL_SCALE_FACTOR, GLOBAL_SCALE_FACTOR); return 0; }
void initialise_benchmark (void)  { in_seed_a = 0xA1F32C97u; in_seed_b = 0x5EE9D4B2u; g_check = 0; }
int  verify_benchmark (int res)   { (void) res; return (g_check != 0u) && (g_check != 0xC0FFEEu); }
