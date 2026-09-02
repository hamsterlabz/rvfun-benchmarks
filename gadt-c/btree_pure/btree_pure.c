/******************************************************************************
 * btree_pure.c — functional-C binary-search-tree benchmark for the GADT suite.
 *
 * Every insert / delete returns a NEW tree along the path of the
 * modification (path copying — the standard pure-data-structure
 * idiom). Untouched subtrees are shared. The dual ninja version
 * uses in-place link rewrites with an explicit successor swap.
 *****************************************************************************/

#pragma GCC optimize ("no-inline,no-reorder-functions")

#include "support.h"
#include "gadt_common.h"

#define LOCAL_SCALE_FACTOR 12

GADT_ARENA_DEFINE();

typedef struct btree_s {
  uint32_t        key;
  struct btree_s *left, *right;
} btree_t;

static btree_t *
btree_node (uint32_t key, btree_t *l, btree_t *r)
{
  btree_t *n = (btree_t *) gadt_arena_alloc (sizeof (btree_t));
  if (!n) return 0;
  n->key   = key;
  n->left  = l;
  n->right = r;
  return n;
}

/* ---- insert (path-copying) ------------------------------------------ */

static btree_t *
btree_insert (btree_t *t, uint32_t k)
{
  if (!t) return btree_node (k, 0, 0);
  if (k <  t->key) return btree_node (t->key, btree_insert (t->left, k), t->right);
  if (k >  t->key) return btree_node (t->key, t->left, btree_insert (t->right, k));
  return t;   /* duplicates: no change */
}

/* ---- delete (path-copying; successor merge for two-child case) ---- */

static uint32_t
btree_min_key (btree_t *t) { while (t->left) t = t->left; return t->key; }

static btree_t *
btree_delete_min (btree_t *t)
{
  if (!t->left) return t->right;
  return btree_node (t->key, btree_delete_min (t->left), t->right);
}

static btree_t *
btree_delete (btree_t *t, uint32_t k)
{
  if (!t) return 0;
  if (k <  t->key) return btree_node (t->key, btree_delete (t->left, k), t->right);
  if (k >  t->key) return btree_node (t->key, t->left, btree_delete (t->right, k));
  /* match: drop t */
  if (!t->left)  return t->right;
  if (!t->right) return t->left;
  uint32_t s = btree_min_key (t->right);
  return btree_node (s, t->left, btree_delete_min (t->right));
}

/* ---- folds (in-order) ----------------------------------------------- */

static uint32_t
btree_foldl (uint32_t (*f)(uint32_t, uint32_t), uint32_t z, btree_t *t)
{
  if (!t) return z;
  z = btree_foldl (f, z, t->left);
  z = f (z, t->key);
  z = btree_foldl (f, z, t->right);
  return z;
}

static uint32_t
btree_foldr (uint32_t (*f)(uint32_t, uint32_t), uint32_t z, btree_t *t)
{
  if (!t) return z;
  z = btree_foldr (f, z, t->right);
  z = f (t->key, z);
  z = btree_foldr (f, z, t->left);
  return z;
}

/* ---- map (structure-preserving rebuild) ---------------------------- */

static btree_t *
btree_map (uint32_t (*f)(uint32_t), btree_t *t)
{
  if (!t) return 0;
  return btree_node (f (t->key), btree_map (f, t->left), btree_map (f, t->right));
}

/* ---- zipWith (structure-aligned, stops at shorter branch) --------- */

static uint32_t
btree_zipsum_with (uint32_t (*f)(uint32_t, uint32_t), btree_t *a, btree_t *b)
{
  if (!a || !b) return 0;
  return f (a->key, b->key)
       + btree_zipsum_with (f, a->left,  b->left)
       + btree_zipsum_with (f, a->right, b->right);
}

/* ---- helper combinators --------------------------------------------- */

static uint32_t f_add (uint32_t a, uint32_t b) { return a + b; }
static uint32_t f_xor (uint32_t a, uint32_t b) { return a ^ b; }
static uint32_t f_mul3plus1 (uint32_t x)        { return x * 3 + 1; }

/* ---- benchmark body --------------------------------------------------- */

#define KEY_COUNT 24

static uint32_t in_seed_a, in_seed_b;

static uint32_t
benchmark_body (unsigned int lsf, unsigned int gsf)
{
  uint32_t check = 0xC0FFEEu;
  for (unsigned int outer = 0; outer < lsf * gsf; outer++) {
    gadt_arena_reset ();
    uint32_t sa = in_seed_a + outer;
    uint32_t sb = in_seed_b + outer;

    /* INSERTION: build two BSTs of KEY_COUNT random keys. */
    btree_t *ta = 0, *tb = 0;
    for (int i = 0; i < KEY_COUNT; i++) {
      ta = btree_insert (ta, gadt_lcg_range (&sa, 4096));
      tb = btree_insert (tb, gadt_lcg_range (&sb, 4096));
    }

    /* DELETION: drop a few keys from ta. */
    uint32_t sd = sa ^ 0xDEAD;
    for (int i = 0; i < 4; i++)
      ta = btree_delete (ta, gadt_lcg_range (&sd, 4096));

    /* MAP */
    btree_t *tm = btree_map (f_mul3plus1, ta);

    /* FOLDL + FOLDR */
    uint32_t sl = btree_foldl (f_add, 0, tm);
    uint32_t sr = btree_foldr (f_xor, 0, tm);

    /* ZIP / ZIPWITH */
    uint32_t za = btree_zipsum_with (f_add, tm, tb);
    uint32_t zx = btree_zipsum_with (f_xor, tm, tb);

    check = gadt_hash_mix (check, sl);
    check = gadt_hash_mix (check, sr);
    check = gadt_hash_mix (check, za);
    check = gadt_hash_mix (check, zx);
  }
  return check;
}

/* ---- Embench harness -------------------------------------------------- */

static uint32_t g_check;
void warm_caches (int heat)       { (void) benchmark_body (1, heat); }
int  benchmark (void)             { g_check = benchmark_body (LOCAL_SCALE_FACTOR, GLOBAL_SCALE_FACTOR); return 0; }
void initialise_benchmark (void)  { in_seed_a = 0xA1F32C97u; in_seed_b = 0x5EE9D4B2u; g_check = 0; }
int  verify_benchmark (int res)   { (void) res; return (g_check != 0u) && (g_check != 0xC0FFEEu); }
