/******************************************************************************
 * msort_pure.c — direct recursive merge sort on a path-copying
 * cons-list.  Split the list in half, sort each, merge.
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

static uint32_t
list_len (cell_t *xs) { uint32_t k = 0; for (; xs; xs = xs->n) k++; return k; }

/* Split at index `k`: returns (left, right). */
static cell_t *
split_at (cell_t *xs, uint32_t k, cell_t **right_out)
{
  if (k == 0) { *right_out = xs; return 0; }
  cell_t *left = 0, **tail = &left;
  while (k > 0 && xs) {
    *tail = cons (xs->v, 0);
    tail = &(*tail)->n;
    xs = xs->n;
    k--;
  }
  *right_out = xs;
  return left;
}

static cell_t *
merge_lists (cell_t *a, cell_t *b)
{
  if (!a) return b;
  if (!b) return a;
  if (a->v <= b->v) return cons (a->v, merge_lists (a->n, b));
  return cons (b->v, merge_lists (a, b->n));
}

static cell_t *
msort (cell_t *xs)
{
  uint32_t n = list_len (xs);
  if (n < 2) return xs;
  cell_t *right;
  cell_t *left = split_at (xs, n / 2, &right);
  return merge_lists (msort (left), msort (right));
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
    cell_t *sorted = msort (xs);
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
