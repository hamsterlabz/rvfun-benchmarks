/******************************************************************************
 * quadtree_pure.c — functional-C quadtree benchmark for the GADT suite.
 *
 * 2-bit address scheme: each node has 4 children indexed by (NW/NE/SW/SE),
 * insertion walks the address bits MSB-first. Pure variant: every op
 * returns a fresh tree along the path of the modification (path
 * copying); untouched quadrants are shared.
 *****************************************************************************/

#pragma GCC optimize ("no-inline,no-reorder-functions")

#include "support.h"
#include "gadt_common.h"

#define LOCAL_SCALE_FACTOR 6

GADT_ARENA_DEFINE();

typedef struct qt_s {
  uint32_t       val;          /* payload at this node (0 if interior-only) */
  struct qt_s   *kid[4];       /* NW=0, NE=1, SW=2, SE=3 */
} qt_t;

static qt_t *
qt_leaf (uint32_t v)
{
  qt_t *n = (qt_t *) gadt_arena_alloc (sizeof (qt_t));
  if (!n) return 0;
  n->val = v;
  for (int i = 0; i < 4; i++) n->kid[i] = 0;
  return n;
}

/* Allocate a fresh node copying `t`'s structure but overriding the
 * given quadrant pointer + val. Used by the path-copying insert. */
static qt_t *
qt_copy_with (qt_t *t, int q, qt_t *new_kid, uint32_t v)
{
  qt_t *n = (qt_t *) gadt_arena_alloc (sizeof (qt_t));
  if (!n) return t;
  n->val = (t ? t->val : v);
  if (!t) for (int i = 0; i < 4; i++) n->kid[i] = 0;
  else    for (int i = 0; i < 4; i++) n->kid[i] = t->kid[i];
  if (q >= 0) n->kid[q] = new_kid;
  return n;
}

/* ---- insert (path-copy) — address = 8 bits, MSB-first 4 levels deep */

#define QT_DEPTH 4

static qt_t *
qt_insert_at (qt_t *t, uint32_t addr, int depth_left, uint32_t v)
{
  if (depth_left == 0) {
    /* Terminal: write payload (path-copy if t exists). */
    if (!t) return qt_leaf (v);
    qt_t *n = qt_copy_with (t, -1, 0, v);
    if (n) n->val = v;
    return n;
  }
  int q = (addr >> ((depth_left - 1) * 2)) & 0x3;
  qt_t *kid = t ? t->kid[q] : 0;
  qt_t *new_kid = qt_insert_at (kid, addr, depth_left - 1, v);
  if (!t) return qt_copy_with (0, q, new_kid, 0);
  return qt_copy_with (t, q, new_kid, t->val);
}

static qt_t *
qt_insert (qt_t *t, uint32_t addr, uint32_t v)
{
  return qt_insert_at (t, addr, QT_DEPTH, v);
}

/* ---- "deletion" = path-copy, zero out the leaf payload at addr -- */

static qt_t *
qt_delete_at (qt_t *t, uint32_t addr, int depth_left)
{
  if (!t) return 0;
  if (depth_left == 0) {
    qt_t *n = qt_copy_with (t, -1, 0, 0);
    if (n) n->val = 0;
    return n;
  }
  int q = (addr >> ((depth_left - 1) * 2)) & 0x3;
  qt_t *kid = qt_delete_at (t->kid[q], addr, depth_left - 1);
  return qt_copy_with (t, q, kid, t->val);
}

static qt_t *
qt_delete (qt_t *t, uint32_t addr)
{
  return qt_delete_at (t, addr, QT_DEPTH);
}

/* ---- folds ---------------------------------------------------------- */

static uint32_t
qt_foldl (uint32_t (*f)(uint32_t, uint32_t), uint32_t z, qt_t *t)
{
  if (!t) return z;
  z = f (z, t->val);
  for (int i = 0; i < 4; i++) z = qt_foldl (f, z, t->kid[i]);
  return z;
}

static uint32_t
qt_foldr (uint32_t (*f)(uint32_t, uint32_t), uint32_t z, qt_t *t)
{
  if (!t) return z;
  for (int i = 3; i >= 0; i--) z = qt_foldr (f, z, t->kid[i]);
  return f (t->val, z);
}

/* ---- map ------------------------------------------------------------ */

static qt_t *
qt_map (uint32_t (*f)(uint32_t), qt_t *t)
{
  if (!t) return 0;
  qt_t *n = (qt_t *) gadt_arena_alloc (sizeof (qt_t));
  if (!n) return 0;
  n->val = f (t->val);
  for (int i = 0; i < 4; i++) n->kid[i] = qt_map (f, t->kid[i]);
  return n;
}

/* ---- zipWith ------------------------------------------------------- */

static uint32_t
qt_zipsum_with (uint32_t (*f)(uint32_t, uint32_t), qt_t *a, qt_t *b)
{
  if (!a || !b) return 0;
  uint32_t s = f (a->val, b->val);
  for (int i = 0; i < 4; i++) s += qt_zipsum_with (f, a->kid[i], b->kid[i]);
  return s;
}

/* ---- combinators ---------------------------------------------------- */

static uint32_t f_add (uint32_t a, uint32_t b) { return a + b; }
static uint32_t f_xor (uint32_t a, uint32_t b) { return a ^ b; }
static uint32_t f_mul3plus1 (uint32_t x)        { return x * 3 + 1; }

/* ---- benchmark body ------------------------------------------------- */

#define INSERT_COUNT 24

static uint32_t in_seed_a, in_seed_b;

static uint32_t
benchmark_body (unsigned int lsf, unsigned int gsf)
{
  uint32_t check = 0xC0FFEEu;
  for (unsigned int outer = 0; outer < lsf * gsf; outer++) {
    gadt_arena_reset ();
    uint32_t sa = in_seed_a + outer;
    uint32_t sb = in_seed_b + outer;
    uint32_t sa0 = sa;            /* original seed: dels derive from THIS,
                                     not the insert-advanced sa (matches the
                                     Haskell `sa` binding, which is pure). */

    qt_t *ta = 0, *tb = 0;
    for (int i = 0; i < INSERT_COUNT; i++) {
      uint32_t addr_a = gadt_lcg_next (&sa) & 0xFF;
      uint32_t addr_b = gadt_lcg_next (&sb) & 0xFF;
      ta = qt_insert (ta, addr_a, gadt_lcg_next (&sa));
      tb = qt_insert (tb, addr_b, gadt_lcg_next (&sb));
    }

    /* mkOps semantics: each del op consumes TWO lcg steps (addr from the
       first, value from the second/discarded), threading the second. */
    uint32_t sd = sa0 ^ 0xCAFE;
    for (int i = 0; i < 4; i++) {
      uint32_t addr_d = gadt_lcg_next (&sd) & 0xFF;
      (void) gadt_lcg_next (&sd);            /* discarded value step */
      ta = qt_delete (ta, addr_d);
    }

    qt_t *tm = qt_map (f_mul3plus1, ta);

    uint32_t sl = qt_foldl (f_add, 0, tm);
    uint32_t sr = qt_foldr (f_xor, 0, tm);

    uint32_t za = qt_zipsum_with (f_add, tm, tb);
    uint32_t zx = qt_zipsum_with (f_xor, tm, tb);

    check = gadt_hash_mix (check, sl);
    check = gadt_hash_mix (check, sr);
    check = gadt_hash_mix (check, za);
    check = gadt_hash_mix (check, zx);
  }
  return check;
}

/* ---- harness -------------------------------------------------------- */

static uint32_t g_check;
void warm_caches (int heat)       { (void) benchmark_body (1, heat); }
int  benchmark (void)             { g_check = benchmark_body (LOCAL_SCALE_FACTOR, GLOBAL_SCALE_FACTOR); return 0; }
void initialise_benchmark (void)  { in_seed_a = 0xA1F32C97u; in_seed_b = 0x5EE9D4B2u; g_check = 0; }
int  verify_benchmark (int res)   { (void) res; return (g_check != 0u) && (g_check != 0xC0FFEEu); }
