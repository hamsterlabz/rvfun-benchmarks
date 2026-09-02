/******************************************************************************
 * tuple_pure.c — functional-C pair/triple benchmark for the GADT suite.
 *
 * Tuples are fixed-size, so "insertion" = construct, "deletion" =
 * project a single component, "fold" = reduce the components, "map"
 * = transform each component, "zip" = pair two tuples component-wise.
 *
 * Pure variant: every operation returns a NEW tuple by value (small
 * struct pass — caller copy). No mutation. The dual ninja version
 * pins tuples in registers and uses fused ops where it can.
 *****************************************************************************/

#pragma GCC optimize ("no-inline,no-reorder-functions")

#include "support.h"
#include "gadt_common.h"

#define LOCAL_SCALE_FACTOR 1024

GADT_ARENA_DEFINE();   /* unused but keeps the linker symbol set uniform */

/* The benchmark exercises a 3-tuple. Pair (2-tuple) ops are a strict
 * subset and would just be #defines around triples — not interesting
 * separately. */
typedef struct { uint32_t a, b, c; } triple_t;

/* ---- constructors / projections (pure) ------------------------------ */

static triple_t
triple_mk (uint32_t a, uint32_t b, uint32_t c)
{
  triple_t t = { a, b, c };
  return t;
}

static uint32_t triple_fst (triple_t t) { return t.a; }
static uint32_t triple_snd (triple_t t) { return t.b; }
static uint32_t triple_trd (triple_t t) { return t.c; }

/* ---- "deletion" — drop one slot, return the residual pair packed
 * into a triple (slot reset to 0). The C type doesn't admit a
 * 2-tuple-from-a-3-tuple shrink, so this is the closest pure idiom.
 */
static triple_t
triple_drop_first (triple_t t) { return triple_mk (0, t.b, t.c); }

/* ---- folds ------------------------------------------------------------ */

/* "foldr" on a triple = f a (f b (f c z))  */
static uint32_t
triple_foldr (uint32_t (*f)(uint32_t, uint32_t), uint32_t z, triple_t t)
{
  return f (t.a, f (t.b, f (t.c, z)));
}

/* "foldl" on a triple = f (f (f z a) b) c */
static uint32_t
triple_foldl (uint32_t (*f)(uint32_t, uint32_t), uint32_t z, triple_t t)
{
  return f (f (f (z, t.a), t.b), t.c);
}

/* ---- map -------------------------------------------------------------- */

static triple_t
triple_map (uint32_t (*f)(uint32_t), triple_t t)
{
  return triple_mk (f (t.a), f (t.b), f (t.c));
}

/* ---- zip / zipWith ---------------------------------------------------- */

static triple_t
triple_zip_with (uint32_t (*f)(uint32_t, uint32_t), triple_t x, triple_t y)
{
  return triple_mk (f (x.a, y.a), f (x.b, y.b), f (x.c, y.c));
}

/* ---- helper combinators --------------------------------------------- */

static uint32_t f_add (uint32_t a, uint32_t b) { return a + b; }
static uint32_t f_xor (uint32_t a, uint32_t b) { return a ^ b; }
static uint32_t f_mul3plus1 (uint32_t x)        { return x * 3 + 1; }

/* ---- benchmark body --------------------------------------------------- */

static uint32_t in_seed_a, in_seed_b;

static uint32_t
benchmark_body (unsigned int lsf, unsigned int gsf)
{
  uint32_t check = 0xC0FFEEu;
  for (unsigned int outer = 0; outer < lsf * gsf; outer++) {

    /* INSERTION: build two 3-tuples from fresh LCG samples. Reseed per
     * iteration (sa = in_seed_a + outer) and derive y's seed from x's
     * final state (+ in_seed_b), matching the Haskell `draw3` chain.
     * Sequence the draws explicitly — C arg-eval order is unspecified. */
    uint32_t sa = in_seed_a + outer;
    uint32_t xa = gadt_lcg_next (&sa);
    uint32_t xb = gadt_lcg_next (&sa);
    uint32_t xc = gadt_lcg_next (&sa);
    triple_t x = triple_mk (xa, xb, xc);
    uint32_t sb = sa + in_seed_b;
    uint32_t ya = gadt_lcg_next (&sb);
    uint32_t yb = gadt_lcg_next (&sb);
    uint32_t yc = gadt_lcg_next (&sb);
    triple_t y = triple_mk (ya, yb, yc);

    /* DELETION (slot-zero variant). Probe each component projection
     * so the compiler can't dead-code-eliminate the build. */
    triple_t xd = triple_drop_first (x);
    uint32_t pj = triple_fst (xd) ^ triple_snd (xd) ^ triple_trd (xd);

    /* MAP */
    triple_t xm = triple_map (f_mul3plus1, x);

    /* FOLDL + FOLDR */
    uint32_t sl = triple_foldl (f_add, 0, xm);
    uint32_t sr = triple_foldr (f_xor, 0, xm);

    /* ZIP / ZIPWITH */
    triple_t z_add = triple_zip_with (f_add, xm, y);
    triple_t z_xor = triple_zip_with (f_xor, xm, y);
    uint32_t za = z_add.a ^ z_add.b ^ z_add.c;
    uint32_t zx = z_xor.a ^ z_xor.b ^ z_xor.c;

    check = gadt_hash_mix (check, sl);
    check = gadt_hash_mix (check, sr);
    check = gadt_hash_mix (check, pj);
    check = gadt_hash_mix (check, za);
    check = gadt_hash_mix (check, zx);
  }
  return check;
}

/* ---- Embench harness -------------------------------------------------- */

static uint32_t g_check;

void warm_caches (int heat)
{ (void) benchmark_body (1, heat); }

int benchmark (void)
{ g_check = benchmark_body (LOCAL_SCALE_FACTOR, GLOBAL_SCALE_FACTOR); return 0; }

void initialise_benchmark (void)
{ in_seed_a = 0xA1F32C97u; in_seed_b = 0x5EE9D4B2u; g_check = 0; }

int verify_benchmark (int res)
{ (void) res; return (g_check != 0u) && (g_check != 0xC0FFEEu); }
