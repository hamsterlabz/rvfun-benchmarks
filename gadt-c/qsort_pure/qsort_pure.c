/******************************************************************************
 * qsort_pure.c — direct recursive quicksort on a cons-list.
 * Pivot = head; partition the tail into < and >=.
 *****************************************************************************/

#pragma GCC optimize ("no-inline,no-reorder-functions")

#include "support.h"
#include "gadt_common.h"

#define LOCAL_SCALE_FACTOR 32

GADT_ARENA_DEFINE();

typedef struct cell_s { uint32_t v; struct cell_s *n; } cell_t;

static cell_t *
cons (uint32_t v, cell_t *n)
{
  cell_t *c = (cell_t *) gadt_arena_alloc (sizeof (cell_t));
  if (!c) return n;
  c->v = v; c->n = n; return c;
}

static cell_t *
append (cell_t *a, cell_t *b)
{
  if (!a) return b;
  return cons (a->v, append (a->n, b));
}

static cell_t *
qsort_ (cell_t *xs)
{
  if (!xs) return 0;
  uint32_t p = xs->v;
  cell_t *lt = 0, *ge = 0;
  for (cell_t *c = xs->n; c; c = c->n) {
    if (c->v <  p) lt = cons (c->v, lt);
    else           ge = cons (c->v, ge);
  }
  return append (qsort_ (lt), cons (p, qsort_ (ge)));
}

static cell_t *
draw_lcg (int n, uint32_t s)
{
  cell_t *r = 0;
  for (int i = 0; i < n; i++) { s = gadt_lcg_next (&s); r = cons (s, r); }
  return r;
}

static uint32_t head_v (cell_t *xs) { return xs ? xs->v : 0; }
static uint32_t last_v (cell_t *xs)
{ if (!xs) return 0; while (xs->n) xs = xs->n; return xs->v; }
static uint32_t sum_list (cell_t *xs) { uint32_t s = 0; for (; xs; xs = xs->n) s += xs->v; return s; }
static uint32_t pos_hash (cell_t *xs) { uint32_t a = 0; for (; xs; xs = xs->n) a = a * 31u + xs->v; return a; }

#define LIST_LEN 64

static uint32_t in_seed_a;

static uint32_t
benchmark_body (unsigned int lsf, unsigned int gsf)
{
  uint32_t check = 0xC0FFEEu;
  for (unsigned int outer = 0; outer < lsf * gsf; outer++) {
    gadt_arena_reset ();
    uint32_t sa = in_seed_a + outer;
    cell_t *xs     = draw_lcg (LIST_LEN, sa);
    cell_t *sorted = qsort_ (xs);
    check = gadt_hash_mix (check, head_v (sorted));
    check = gadt_hash_mix (check, last_v (sorted));
    check = gadt_hash_mix (check, sum_list (sorted));
    check = gadt_hash_mix (check, pos_hash (sorted));
  }
  return check;
}

static uint32_t g_check;
void warm_caches (int heat)       { (void) benchmark_body (1, heat); }
int  benchmark (void)             { g_check = benchmark_body (LOCAL_SCALE_FACTOR, GLOBAL_SCALE_FACTOR); return 0; }
void initialise_benchmark (void)  { in_seed_a = 0xA1F32C97u; g_check = 0; }
int  verify_benchmark (int res)   { (void) res; return (g_check != 0u) && (g_check != 0xC0FFEEu); }
