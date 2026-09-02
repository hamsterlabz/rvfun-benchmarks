/******************************************************************************
 * church_pure.c — Church / Peano numerals, direct path-copying recursion.
 *
 * Following the nofib `exp3_8` test: every Nat is a linked list of
 * `S` cells terminating in NULL (= Z).  Operations (plus / times /
 * exp / pred) are iterative for stack safety — exp 3 8 = 6561 makes
 * recursive Peano arithmetic blow rv0's stack.
 *
 *   data Nat = Z | S Nat
 *   plus  a b = repeat (S) |a| times on b
 *   times a b = repeat (plus b) |a| times on Z
 *   exp   a b = repeat (times a) |b| times on (S Z)
 *   pred  Z   = Z
 *   pred  (S n) = n
 *****************************************************************************/

#pragma GCC optimize ("no-inline,no-reorder-functions")

#define GADT_ARENA_SIZE  (128 * 1024)        /* fits exp 3 8 = 6561 cells */
#include "support.h"
#include "gadt_common.h"

#define LOCAL_SCALE_FACTOR 1

GADT_ARENA_DEFINE();

/* Nat: NULL = Z; non-NULL pointer = S of predecessor. */
typedef struct nat_s {
  struct nat_s *pred;
} nat_t;

static nat_t *
nat_s (nat_t *n)
{
  nat_t *c = (nat_t *) gadt_arena_alloc (sizeof (nat_t));
  if (!c) return n;
  c->pred = n;
  return c;
}

static uint32_t
nat_count (nat_t *n)
{
  uint32_t k = 0;
  for (; n; n = n->pred) k++;
  return k;
}

static nat_t *
int_to_nat (uint32_t k)
{
  nat_t *r = 0;
  while (k > 0) { r = nat_s (r); k--; }
  return r;
}

static uint32_t nat_to_int (nat_t *n) { return nat_count (n); }

/* plus a b = repeat S |a| times on b. */
static nat_t *
plus_nat (nat_t *a, nat_t *b)
{
  nat_t *r = b;
  for (nat_t *p = a; p; p = p->pred) r = nat_s (r);
  return r;
}

/* times a b = repeat (plus b) |a| times on Z. */
static nat_t *
times_nat (nat_t *a, nat_t *b)
{
  nat_t *r = 0;
  for (nat_t *p = a; p; p = p->pred) r = plus_nat (b, r);
  return r;
}

/* exp a b = repeat (times a) |b| times on (S Z). */
static nat_t *
exp_nat (nat_t *a, nat_t *b)
{
  nat_t *r = nat_s (0);                /* S Z = 1 */
  for (nat_t *p = b; p; p = p->pred) r = times_nat (a, r);
  return r;
}

static nat_t * pred_nat (nat_t *n) { return n ? n->pred : 0; }

/* ---- benchmark body --------------------------------------------- */

static uint32_t in_seed_a;

static uint32_t
benchmark_body (unsigned int lsf, unsigned int gsf)
{
  uint32_t check = 0xC0FFEEu;
  for (unsigned int outer = 0; outer < lsf * gsf; outer++) {
    gadt_arena_reset ();
    uint32_t sa = outer;

    nat_t *three = int_to_nat (3);
    nat_t *eight = int_to_nat (8);
    nat_t *a     = int_to_nat ((sa     % 6) + 2);
    nat_t *b     = int_to_nat ((sa % 4) + 3);

    uint32_t e  = nat_to_int (exp_nat   (three, eight));
    uint32_t pn = nat_to_int (plus_nat  (a, b));
    uint32_t tn = nat_to_int (times_nat (a, b));
    uint32_t dn = nat_to_int (pred_nat  (a));

    check = gadt_hash_mix (check, e);
    check = gadt_hash_mix (check, pn);
    check = gadt_hash_mix (check, tn);
    check = gadt_hash_mix (check, dn);
  }
  return check;
}

/* ---- harness ---------------------------------------------------- */

static uint32_t g_check;
void warm_caches (int heat)       { (void) benchmark_body (1, heat); }
int  benchmark (void)             { g_check = benchmark_body (LOCAL_SCALE_FACTOR, GLOBAL_SCALE_FACTOR); return 0; }
void initialise_benchmark (void)  { in_seed_a = 0xA1F32C97u; g_check = 0; }
int  verify_benchmark (int res)   { (void) res; return (g_check != 0u) && (g_check != 0xC0FFEEu); }
