/******************************************************************************
 * hsort_pure.c — heap sort via skew heap.  build = foldr insert HE;
 * drain = extract-min repeatedly until empty.
 *****************************************************************************/

#pragma GCC optimize ("no-inline,no-reorder-functions")

#include "support.h"
#include "gadt_common.h"

#define LOCAL_SCALE_FACTOR 32

GADT_ARENA_DEFINE();

typedef struct cell_s { uint32_t v; struct cell_s *n; } cell_t;
typedef struct heap_s { uint32_t v; struct heap_s *l, *r; } heap_t;

static cell_t *
cons (uint32_t v, cell_t *n)
{
  cell_t *c = (cell_t *) gadt_arena_alloc (sizeof (cell_t));
  if (!c) return n;
  c->v = v; c->n = n; return c;
}

static heap_t *
hnode (uint32_t v, heap_t *l, heap_t *r)
{
  heap_t *h = (heap_t *) gadt_arena_alloc (sizeof (heap_t));
  if (!h) return 0;
  h->v = v; h->l = l; h->r = r; return h;
}

static heap_t *
merge_h (heap_t *a, heap_t *b)
{
  if (!a) return b;
  if (!b) return a;
  if (a->v <= b->v) return hnode (a->v, merge_h (a->r, b), a->l);
  return hnode (b->v, merge_h (a, b->r), b->l);
}

static heap_t * insert_h (uint32_t x, heap_t *h) { return merge_h (hnode (x, 0, 0), h); }

static cell_t *
drain (heap_t *h)
{
  if (!h) return 0;
  return cons (h->v, drain (merge_h (h->l, h->r)));
}

static heap_t *
build (cell_t *xs)
{
  if (!xs) return 0;
  return insert_h (xs->v, build (xs->n));
}

static cell_t *
hsort (cell_t *xs) { return drain (build (xs)); }

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
    cell_t *sorted = hsort (xs);
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
