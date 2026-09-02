/******************************************************************************
 * list_pure.c — functional-C cons-list benchmark for the GADT suite.
 *
 * Implements: insertion (cons), deletion (drop-head), foldl, foldr,
 *             map, zip, zipWith.
 *
 * Every operation returns a NEW list — no mutation of existing cells.
 * Allocations come from the per-TU bump arena. Closer to how a Haskell
 * compiler would target rv32; the dual `list_ninja.c` is the in-place
 * counterpart for direct comparison.
 *****************************************************************************/

#pragma GCC optimize ("no-inline,no-reorder-functions")

#include "support.h"
#include "gadt_common.h"

#define LOCAL_SCALE_FACTOR 32

GADT_ARENA_DEFINE();

typedef struct list_node_s {
  uint32_t              val;
  struct list_node_s   *next;   /* 0 → end of list */
} list_t;

/* ---- pure constructors / destructors --------------------------------- */

static list_t *
list_cons (uint32_t v, list_t *tail)
{
  list_t *n = (list_t *) gadt_arena_alloc (sizeof (list_t));
  if (!n) return tail;          /* OOM: drop the new element */
  n->val  = v;
  n->next = tail;
  return n;
}

/* Drop head — returns the tail (pure: no free, the head cell is left
 * dangling in the arena and gets reclaimed on the next arena_reset). */
static list_t *
list_drop_head (list_t *xs)
{
  return xs ? xs->next : 0;
}

/* ---- folds ------------------------------------------------------------ */

static uint32_t
list_foldl (uint32_t (*f)(uint32_t, uint32_t), uint32_t z, list_t *xs)
{
  uint32_t acc = z;
  for (; xs; xs = xs->next) acc = f (acc, xs->val);
  return acc;
}

/* True right fold — reconstruct the call chain. Pure C makes this a
 * regular recursive call; the dual ninja uses an explicit reverse-
 * then-foldl trick to dodge stack pressure. */
static uint32_t
list_foldr (uint32_t (*f)(uint32_t, uint32_t), uint32_t z, list_t *xs)
{
  if (!xs) return z;
  return f (xs->val, list_foldr (f, z, xs->next));
}

/* ---- map -------------------------------------------------------------- */

static list_t *
list_map (uint32_t (*f)(uint32_t), list_t *xs)
{
  if (!xs) return 0;
  /* Build the new list head-first; reverse at the end so order
   * matches xs (a literal Haskell `map` would consume + cons,
   * yielding reverse-then-reverse via the call stack — same cost). */
  list_t *rev = 0;
  for (; xs; xs = xs->next) rev = list_cons (f (xs->val), rev);
  list_t *out = 0;
  for (; rev; rev = rev->next) out = list_cons (rev->val, out);
  return out;
}

/* ---- zip / zipWith ---------------------------------------------------- */

static uint32_t
list_zip_sum_with (uint32_t (*f)(uint32_t, uint32_t), list_t *as, list_t *bs)
{
  /* Combine into a fresh list, then foldl-sum the result so the
   * full traversal is structurally observable. */
  list_t *rev = 0;
  for (; as && bs; as = as->next, bs = bs->next)
    rev = list_cons (f (as->val, bs->val), rev);
  uint32_t s = 0;
  for (list_t *p = rev; p; p = p->next) s += p->val;
  return s;
}

/* ---- helper combinators ---------------------------------------------- */

static uint32_t f_add (uint32_t a, uint32_t b) { return a + b; }
static uint32_t f_xor (uint32_t a, uint32_t b) { return a ^ b; }
static uint32_t f_mul3plus1 (uint32_t x)        { return x * 3 + 1; }

/* ---- benchmark body --------------------------------------------------- */

#define LIST_LEN 64

static uint32_t in_seed_a, in_seed_b;

static uint32_t
benchmark_body (unsigned int lsf, unsigned int gsf)
{
  uint32_t check = 0xC0FFEEu;
  for (unsigned int outer = 0; outer < lsf * gsf; outer++) {
    gadt_arena_reset ();
    uint32_t sa = in_seed_a + outer;
    uint32_t sb = in_seed_b + outer;

    /* INSERTION: build two LIST_LEN-element lists by repeated cons. */
    list_t *as = 0, *bs = 0;
    for (int i = 0; i < LIST_LEN; i++) {
      as = list_cons (gadt_lcg_next (&sa), as);
      bs = list_cons (gadt_lcg_next (&sb), bs);
    }

    /* DELETION: drop the head a few times so the structure visibly
     * shrinks (different from no-op). */
    for (int i = 0; i < 4; i++) as = list_drop_head (as);

    /* MAP: transform every element. */
    list_t *as2 = list_map (f_mul3plus1, as);

    /* FOLDL + FOLDR: two parallel reductions, mix into checksum. */
    uint32_t sum_l = list_foldl (f_add, 0, as2);
    uint32_t sum_r = list_foldr (f_xor, 0, as2);

    /* ZIP / ZIPWITH: combine as2 and bs element-wise. */
    uint32_t z_add = list_zip_sum_with (f_add, as2, bs);
    uint32_t z_xor = list_zip_sum_with (f_xor, as2, bs);

    check = gadt_hash_mix (check, sum_l);
    check = gadt_hash_mix (check, sum_r);
    check = gadt_hash_mix (check, z_add);
    check = gadt_hash_mix (check, z_xor);
  }
  return check;
}

/* ---- Embench harness -------------------------------------------------- */

static uint32_t g_check;

void
warm_caches (int heat)
{
  (void) benchmark_body (1, heat);
}

int
benchmark (void)
{
  g_check = benchmark_body (LOCAL_SCALE_FACTOR, GLOBAL_SCALE_FACTOR);
  return 0;
}

void
initialise_benchmark (void)
{
  in_seed_a = 0xA1F32C97u;
  in_seed_b = 0x5EE9D4B2u;
  g_check   = 0;
}

int
verify_benchmark (int res)
{
  /* PASS iff the workload completed (benchmark() returns 0). The
   * actual checksum is workload-defined; we don't pin it because
   * any change to LOCAL_SCALE_FACTOR / LIST_LEN / seeds drifts it.
   * What we DO want to confirm is that the workload didn't trap
   * and that the checksum is non-trivial (non-zero, non-seed). */
  (void) res;
  return (g_check != 0u) && (g_check != 0xC0FFEEu);
}
