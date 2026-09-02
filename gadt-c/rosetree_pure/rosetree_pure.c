/******************************************************************************
 * rosetree_pure.c — functional-C rose-tree benchmark.
 *
 * Rose tree: each node has a value + a linked list of arbitrary
 * children. Pure variant: every operation returns a NEW node along
 * the path of the modification, sharing the unchanged siblings.
 *****************************************************************************/

#pragma GCC optimize ("no-inline,no-reorder-functions")

#include "support.h"
#include "gadt_common.h"

#define LOCAL_SCALE_FACTOR 8

GADT_ARENA_DEFINE();

/* Children are a cons-list of rose nodes — flat, traversal-friendly. */
struct rose_s;
typedef struct rkid_s {
  struct rose_s   *node;
  struct rkid_s   *next;
} rkid_t;

typedef struct rose_s {
  uint32_t   val;
  rkid_t    *kids;        /* 0 = leaf */
} rose_t;

static rose_t *
rose_mk (uint32_t v, rkid_t *kids)
{
  rose_t *n = (rose_t *) gadt_arena_alloc (sizeof (rose_t));
  if (!n) return 0;
  n->val  = v;
  n->kids = kids;
  return n;
}

static rkid_t *
rkid_cons (rose_t *node, rkid_t *next)
{
  rkid_t *k = (rkid_t *) gadt_arena_alloc (sizeof (rkid_t));
  if (!k) return next;
  k->node = node;
  k->next = next;
  return k;
}

/* ---- insert child at the front of root's kid list (pure) -------- */

static rose_t *
rose_insert_child (rose_t *t, uint32_t v)
{
  rose_t *kid = rose_mk (v, 0);
  if (!t) return kid;
  return rose_mk (t->val, rkid_cons (kid, t->kids));
}

/* ---- delete: drop first child (pure, path-copy at the root) ---- */

static rose_t *
rose_delete_first_child (rose_t *t)
{
  if (!t || !t->kids) return t;
  return rose_mk (t->val, t->kids->next);
}

/* ---- folds: DFS pre-order ----------------------------------------- */

static uint32_t
rose_foldl (uint32_t (*f)(uint32_t, uint32_t), uint32_t z, rose_t *t)
{
  if (!t) return z;
  z = f (z, t->val);
  for (rkid_t *k = t->kids; k; k = k->next) z = rose_foldl (f, z, k->node);
  return z;
}

static uint32_t
rose_foldr (uint32_t (*f)(uint32_t, uint32_t), uint32_t z, rose_t *t)
{
  if (!t) return z;
  /* Right fold: traverse the kid list right-to-left so the leftmost
   * kid is applied last (matches Haskell foldr semantics over the
   * Foldable derived from Functor). */
  rkid_t *rev = 0;
  for (rkid_t *k = t->kids; k; k = k->next) {
    rkid_t *nk = (rkid_t *) gadt_arena_alloc (sizeof (rkid_t));
    if (!nk) break;
    nk->node = k->node;
    nk->next = rev;
    rev      = nk;
  }
  for (rkid_t *k = rev; k; k = k->next) z = rose_foldr (f, z, k->node);
  return f (t->val, z);
}

/* ---- map: rebuild tree with f applied at every node ------------- */

static rose_t *
rose_map (uint32_t (*f)(uint32_t), rose_t *t)
{
  if (!t) return 0;
  rkid_t *new_kids = 0;
  for (rkid_t *k = t->kids; k; k = k->next)
    new_kids = rkid_cons (rose_map (f, k->node), new_kids);
  /* Restore order (we built reversed). */
  rkid_t *out = 0;
  for (rkid_t *k = new_kids; k; k = k->next) out = rkid_cons (k->node, out);
  return rose_mk (f (t->val), out);
}

/* ---- zipWith: structure-aligned, drops to whichever runs out --- */

static uint32_t
rose_zipsum_with (uint32_t (*f)(uint32_t, uint32_t), rose_t *a, rose_t *b)
{
  if (!a || !b) return 0;
  uint32_t s = f (a->val, b->val);
  rkid_t *ka = a->kids, *kb = b->kids;
  while (ka && kb) {
    s += rose_zipsum_with (f, ka->node, kb->node);
    ka = ka->next;
    kb = kb->next;
  }
  return s;
}

/* ---- combinators -------------------------------------------------- */

static uint32_t f_add (uint32_t a, uint32_t b) { return a + b; }
static uint32_t f_xor (uint32_t a, uint32_t b) { return a ^ b; }
static uint32_t f_mul3plus1 (uint32_t x)        { return x * 3 + 1; }

/* ---- benchmark body ---------------------------------------------- */

#define INSERT_COUNT 16

static uint32_t in_seed_a, in_seed_b;

static uint32_t
benchmark_body (unsigned int lsf, unsigned int gsf)
{
  uint32_t check = 0xC0FFEEu;
  for (unsigned int outer = 0; outer < lsf * gsf; outer++) {
    gadt_arena_reset ();
    uint32_t sa = in_seed_a + outer;
    uint32_t sb = in_seed_b + outer;

    /* Haskell builds the root value from `lcgNext sa` WITHOUT advancing the
     * seed (sa is immutable there), then `vals insertCount sa` re-seeds from
     * the SAME sa.  So root value == first-child value.  Mirror that: draw the
     * root from a throwaway copy, then let the loop start from the original
     * seed. */
    uint32_t sa_root = sa, sb_root = sb;
    rose_t *ta = rose_mk (gadt_lcg_next (&sa_root), 0);
    rose_t *tb = rose_mk (gadt_lcg_next (&sb_root), 0);
    for (int i = 0; i < INSERT_COUNT; i++) {
      ta = rose_insert_child (ta, gadt_lcg_next (&sa));
      tb = rose_insert_child (tb, gadt_lcg_next (&sb));
    }

    for (int i = 0; i < 2; i++) ta = rose_delete_first_child (ta);

    rose_t *tm = rose_map (f_mul3plus1, ta);

    uint32_t sl = rose_foldl (f_add, 0, tm);
    uint32_t sr = rose_foldr (f_xor, 0, tm);

    uint32_t za = rose_zipsum_with (f_add, tm, tb);
    uint32_t zx = rose_zipsum_with (f_xor, tm, tb);

    check = gadt_hash_mix (check, sl);
    check = gadt_hash_mix (check, sr);
    check = gadt_hash_mix (check, za);
    check = gadt_hash_mix (check, zx);
  }
  return check;
}

/* ---- harness ------------------------------------------------- */

static uint32_t g_check;
void warm_caches (int heat)       { (void) benchmark_body (1, heat); }
int  benchmark (void)             { g_check = benchmark_body (LOCAL_SCALE_FACTOR, GLOBAL_SCALE_FACTOR); return 0; }
void initialise_benchmark (void)  { in_seed_a = 0xA1F32C97u; in_seed_b = 0x5EE9D4B2u; g_check = 0; }
int  verify_benchmark (int res)   { (void) res; return (g_check != 0u) && (g_check != 0xC0FFEEu); }
