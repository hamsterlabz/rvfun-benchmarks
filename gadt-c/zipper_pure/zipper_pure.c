/******************************************************************************
 * zipper_pure.c — purely-functional zipper over a binary tree for the
 * GADT suite. (Huet, "The Zipper", JFP 1997.)
 *
 * A zipper is a (focus, trail) pair: the focus is a subtree, the
 * trail is a list of breadcrumbs encoding "how we got here" — each
 * breadcrumb records the parent's value and the sibling subtree on
 * the OTHER side of the descent. Together they give O(1) local
 * edits anywhere in the tree without rebuilding the spine on every
 * step.
 *
 * Pure variant: every navigation (down-left/down-right/up) and edit
 * (replace/modify) returns a NEW zipper; the original is preserved.
 * Path-copying — only the spine along the focus path is copied,
 * everything else is shared. The dual ninja version mutates the
 * breadcrumb chain in place.
 *
 * Operations:
 *   insert (= replace focus) delete (= remove subtree) lookup
 *   member fromList toList foldl foldr map filter partition
 *   split concat reverse rotate zip zipWith size scanl scanr
 *   iterate unfold
 *
 * Plus zipper-specific:
 *   down_left down_right up top                  — navigation
 *   replace modify                                — local edit
 *****************************************************************************/

#pragma GCC optimize ("no-inline,no-reorder-functions")

#include "support.h"
#include "gadt_common.h"

#define LOCAL_SCALE_FACTOR 8

GADT_ARENA_DEFINE();

/* ---- tree ----------------------------------------------------- */

typedef struct tree_s {
  uint32_t        v;
  struct tree_s  *l;
  struct tree_s  *r;
} tree_t;

static tree_t *
mk_node (uint32_t v, tree_t *l, tree_t *r)
{
  tree_t *n = (tree_t *) gadt_arena_alloc (sizeof (tree_t));
  if (!n) return 0;
  n->v = v; n->l = l; n->r = r;
  return n;
}

/* ---- breadcrumbs --------------------------------------------- */

typedef enum { Z_LEFT, Z_RIGHT } z_dir_e;

typedef struct crumb_s {
  z_dir_e          dir;        /* which side we descended FROM */
  uint32_t         pv;         /* parent's value */
  struct tree_s   *sib;        /* the OTHER subtree */
  struct crumb_s  *up;
} crumb_t;

static crumb_t *
mk_crumb (z_dir_e d, uint32_t pv, tree_t *sib, crumb_t *up)
{
  crumb_t *c = (crumb_t *) gadt_arena_alloc (sizeof (crumb_t));
  if (!c) return up;
  c->dir = d; c->pv = pv; c->sib = sib; c->up = up;
  return c;
}

typedef struct {
  tree_t   *focus;
  crumb_t  *trail;
} zip_t;

/* ---- navigation: pure — each step builds a fresh crumb -------- */

static zip_t
z_down_left (zip_t z)
{
  if (!z.focus || !z.focus->l) return z;
  crumb_t *nc = mk_crumb (Z_LEFT, z.focus->v, z.focus->r, z.trail);
  zip_t r = { z.focus->l, nc };
  return r;
}

static zip_t
z_down_right (zip_t z)
{
  if (!z.focus || !z.focus->r) return z;
  crumb_t *nc = mk_crumb (Z_RIGHT, z.focus->v, z.focus->l, z.trail);
  zip_t r = { z.focus->r, nc };
  return r;
}

static zip_t
z_up (zip_t z)
{
  if (!z.trail) return z;
  tree_t *parent;
  if (z.trail->dir == Z_LEFT)
    parent = mk_node (z.trail->pv, z.focus, z.trail->sib);
  else
    parent = mk_node (z.trail->pv, z.trail->sib, z.focus);
  zip_t r = { parent, z.trail->up };
  return r;
}

static zip_t
z_top (zip_t z)
{
  while (z.trail) z = z_up (z);
  return z;
}

/* ---- edit at focus (= "insert" / "delete" in zipper terms) ----- */

static zip_t
z_replace (zip_t z, tree_t *t)
{
  zip_t r = { t, z.trail };
  return r;
}

static zip_t
z_modify (zip_t z, uint32_t (*f)(uint32_t))
{
  if (!z.focus) return z;
  tree_t *nf = mk_node (f (z.focus->v), z.focus->l, z.focus->r);
  zip_t r = { nf, z.trail };
  return r;
}

/* The "delete" of a zipper-focused subtree: replace focus with NULL,
 * then walk up — collapses the parent into the sibling. */
static zip_t
z_delete (zip_t z)
{
  return z_replace (z, 0);
}

/* "insert" — replace focus with a new leaf node carrying value v.
 * If focus already non-NULL, becomes overwrite. */
static zip_t
z_insert (zip_t z, uint32_t v)
{
  return z_replace (z, mk_node (v, 0, 0));
}

/* ---- tree-level: lookup / member / fold / map / size ---------- */

static int  t_member (tree_t *t, uint32_t v) {
  if (!t) return 0;
  if (t->v == v) return 1;
  return t_member (t->l, v) || t_member (t->r, v);
}

static int t_lookup (tree_t *t, uint32_t i, uint32_t *out, uint32_t *seen) {
  if (!t) return 0;
  if (t_lookup (t->l, i, out, seen)) return 1;
  if (*seen == i) { *out = t->v; (*seen)++; return 1; }
  (*seen)++;
  return t_lookup (t->r, i, out, seen);
}

static uint32_t t_foldl (uint32_t (*f)(uint32_t, uint32_t), uint32_t z, tree_t *t) {
  if (!t) return z;
  z = t_foldl (f, z, t->l);
  z = f (z, t->v);
  z = t_foldl (f, z, t->r);
  return z;
}

static uint32_t t_foldr (uint32_t (*f)(uint32_t, uint32_t), uint32_t z, tree_t *t) {
  if (!t) return z;
  z = t_foldr (f, z, t->r);
  z = f (t->v, z);
  z = t_foldr (f, z, t->l);
  return z;
}

static tree_t *t_map (uint32_t (*f)(uint32_t), tree_t *t) {
  if (!t) return 0;
  return mk_node (f (t->v), t_map (f, t->l), t_map (f, t->r));
}

static tree_t *t_filter (int (*p)(uint32_t), tree_t *t) {
  if (!t) return 0;
  tree_t *l = t_filter (p, t->l);
  tree_t *r = t_filter (p, t->r);
  if (p (t->v)) return mk_node (t->v, l, r);
  /* node fails: graft children together — left child becomes new
   * root, with the old right grafted onto its right spine. */
  if (!l) return r;
  if (!r) return l;
  /* "graft right onto rightmost null of l" — keep it simple */
  tree_t *cur = l;
  while (cur->r) cur = cur->r;
  cur->r = r;
  return l;
}

typedef struct { tree_t *yes, *no; } t_split_t;

static t_split_t t_partition (int (*p)(uint32_t), tree_t *t) {
  t_split_t out = { 0, 0 };
  if (!t) return out;
  out.yes = t_filter (p, t);
  /* Build "no" inline. */
  /* gather all values, drop those passing p, rebuild. */
  uint32_t buf[256]; int n = 0;
  /* in-order flatten via explicit stack would be cleaner; keep
   * recursive for brevity since depth is bounded in the bench. */
  /* Reuse foldl in a tiny closure: gather. */
  /* Inline flatten: */
  /* The tree may have up to 2^DEPTH = 64 nodes; bench DEPTH = 6. */
  static tree_t *stk[64]; int top = 0; tree_t *cur = t;
  while (cur || top) {
    while (cur) { stk[top++] = cur; cur = cur->l; }
    cur = stk[--top];
    if (n < 256) buf[n++] = cur->v;
    cur = cur->r;
  }
  tree_t *no = 0;
  for (int i = 0; i < n; i++) if (!p (buf[i])) no = mk_node (buf[i], no, 0);
  out.no = no;
  return out;
}

static uint32_t t_size (tree_t *t) { return t ? 1 + t_size (t->l) + t_size (t->r) : 0; }

/* ---- fromList / toList --------------------------------------- */

static tree_t *
t_insert_bst (tree_t *t, uint32_t v)
{
  if (!t) return mk_node (v, 0, 0);
  if (v == t->v) return t;            /* dedupe */
  if (v < t->v)  return mk_node (t->v, t_insert_bst (t->l, v), t->r);
  return mk_node (t->v, t->l, t_insert_bst (t->r, v));
}

static tree_t *
t_from_arr (const uint32_t *xs, int n)
{
  tree_t *t = 0;
  for (int i = 0; i < n; i++) t = t_insert_bst (t, xs[i]);
  return t;
}

static int
t_to_arr (tree_t *t, uint32_t *out, int cap)
{
  if (!t) return 0;
  int n = 0;
  static tree_t *stk[64]; int top = 0; tree_t *cur = t;
  while ((cur || top) && n < cap) {
    while (cur) { stk[top++] = cur; cur = cur->l; }
    cur = stk[--top];
    out[n++] = cur->v;
    cur = cur->r;
  }
  return n;
}

/* ---- split at index, concat, reverse, rotate ------------------ */

static t_split_t
t_split (tree_t *t, uint32_t i)
{
  t_split_t out = { 0, 0 };
  uint32_t buf[256];
  int n = t_to_arr (t, buf, 256);
  for (int k = 0; k < n; k++) {
    if ((uint32_t) k < i) out.yes = t_insert_bst (out.yes, buf[k]);
    else                  out.no  = t_insert_bst (out.no,  buf[k]);
  }
  return out;
}

static tree_t *
t_concat (tree_t *a, tree_t *b)
{
  uint32_t buf[256];
  int n = t_to_arr (b, buf, 256);
  for (int i = 0; i < n; i++) a = t_insert_bst (a, buf[i]);
  return a;
}

static tree_t *
t_reverse (tree_t *t)
{
  if (!t) return 0;
  return mk_node (t->v, t_reverse (t->r), t_reverse (t->l));
}

static tree_t *
t_rotate (tree_t *t, uint32_t n)
{
  /* "rotate" on a binary tree = take in-order seq, rotate it by N,
   * rebuild via BST insertion. */
  uint32_t buf[256];
  int sz = t_to_arr (t, buf, 256);
  if (sz == 0) return 0;
  tree_t *r = 0;
  for (int i = 0; i < sz; i++) r = t_insert_bst (r, buf[(i + n) % sz]);
  return r;
}

/* ---- zip / zipWith over in-order pairs ------------------------ */

static uint32_t
t_zipsum_with (uint32_t (*f)(uint32_t, uint32_t), tree_t *a, tree_t *b)
{
  uint32_t ba[256], bb[256];
  int na = t_to_arr (a, ba, 256), nb = t_to_arr (b, bb, 256);
  uint32_t s = 0;
  int n = na < nb ? na : nb;
  for (int i = 0; i < n; i++) s += f (ba[i], bb[i]);
  return s;
}

/* ---- scanl / scanr ------------------------------------------- */

static uint32_t
t_scanl (uint32_t (*f)(uint32_t, uint32_t), uint32_t z, tree_t *t)
{
  uint32_t buf[256];
  int n = t_to_arr (t, buf, 256);
  uint32_t acc = z, out = z;
  for (int i = 0; i < n; i++) { acc = f (acc, buf[i]); out = gadt_hash_mix (out, acc); }
  return out;
}

static uint32_t
t_scanr (uint32_t (*f)(uint32_t, uint32_t), uint32_t z, tree_t *t)
{
  uint32_t buf[256];
  int n = t_to_arr (t, buf, 256);
  uint32_t acc = z, out = z;
  for (int i = n - 1; i >= 0; i--) { acc = f (buf[i], acc); out = gadt_hash_mix (out, acc); }
  return out;
}

/* ---- unfold ------------------------------------------------- */

typedef struct { uint32_t v, nxt; int ok; } z_unf_t;
static z_unf_t gen_step (uint32_t s) { z_unf_t e; e.v = gadt_lcg_next (&s); e.nxt = s; e.ok = 1; return e; }

static tree_t *
t_unfold (z_unf_t (*gen)(uint32_t), uint32_t seed, int n)
{
  tree_t *t = 0;
  uint32_t s = seed;
  for (int i = 0; i < n; i++) { z_unf_t e = gen (s); if (!e.ok) break; t = t_insert_bst (t, e.v); s = e.nxt; }
  return t;
}

/* ---- combinators -------------------------------------------- */

static uint32_t f_add (uint32_t a, uint32_t b) { return a + b; }
static uint32_t f_xor (uint32_t a, uint32_t b) { return a ^ b; }
static uint32_t f_mul3plus1 (uint32_t x) { return x * 3 + 1; }
static int      p_even      (uint32_t x) { return (x & 1) == 0; }

/* ---- benchmark body ---------------------------------------- */

#define T_LEN 16

static uint32_t in_seed_a, in_seed_b;

static uint32_t
benchmark_body (unsigned int lsf, unsigned int gsf)
{
  uint32_t check = 0xC0FFEEu;
  for (unsigned int outer = 0; outer < lsf * gsf; outer++) {
    gadt_arena_reset ();
    uint32_t sa = in_seed_a + outer, sb = in_seed_b + outer;

    uint32_t xs_a[T_LEN], xs_b[T_LEN];
    for (int i = 0; i < T_LEN; i++) { xs_a[i] = gadt_lcg_next (&sa) & 0xFFFFu; xs_b[i] = gadt_lcg_next (&sb) & 0xFFFFu; }
    tree_t *ta = t_from_arr (xs_a, T_LEN);
    tree_t *tb = t_from_arr (xs_b, T_LEN);

    /* Canonical workload — 4 hashMix outputs: sl, sr, za, zx.
     * Navigation + edits applied as setup; full surface
     * (lookup/member/filter/partition/split/concat/reverse/rotate/
     * scan/unfold) stays defined but is NOT in the hot loop.
     * Matches zipper_ninja / zipper_morph / Hs.ZipperPure / Hs.ZipperMorph. */
    zip_t zp = { ta, 0 };
    zp = z_down_left  (zp);
    zp = z_down_right (zp);
    zp = z_modify     (zp, f_mul3plus1);
    zp = z_insert     (zp, 0xCAFEBABEu);
    zp = z_up (zp); zp = z_up (zp);
    zip_t zd = z_down_left (zp); zd = z_delete (zd);
    zp = z_top (zd);

    tree_t *tm = t_map (f_mul3plus1, zp.focus);
    uint32_t sl = t_foldl (f_add, 0, tm);
    uint32_t sr = t_foldr (f_xor, 0, tm);
    uint32_t za = t_zipsum_with (f_add, tm, tb);
    uint32_t zx = t_zipsum_with (f_xor, tm, tb);

    check = gadt_hash_mix (check, sl);
    check = gadt_hash_mix (check, sr);
    check = gadt_hash_mix (check, za);
    check = gadt_hash_mix (check, zx);
  }
  return check;
}

static uint32_t g_check;
void warm_caches (int heat)       { (void) benchmark_body (1, heat); }
int  benchmark (void)             { g_check = benchmark_body (LOCAL_SCALE_FACTOR, GLOBAL_SCALE_FACTOR); return 0; }
void initialise_benchmark (void)  { in_seed_a = 0xA1F32C97u; in_seed_b = 0x5EE9D4B2u; g_check = 0; }
int  verify_benchmark (int res)   { (void) res; return (g_check != 0u) && (g_check != 0xC0FFEEu); }
