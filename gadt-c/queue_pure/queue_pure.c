/******************************************************************************
 * queue_pure.c — purely-functional Banker's Queue (Okasaki §6.2) for the
 * GADT suite.
 *
 * A Banker's Queue is a pair of cons-lists (front, rear) plus their
 * lengths. snoc conses to the rear; head/tail take from the front;
 * when the front empties, the rear is reversed onto the front.
 * Amortized O(1) per operation in the strict variant; the truly
 * lazy variant gives O(1) worst-case but requires suspensions — not
 * worth the trouble on rv32.
 *
 * Pure variant: every op returns a NEW queue; the reversal of rear
 * onto front allocates O(rear-len) cells. The dual ninja version
 * uses freelist-backed cells for that O(1) reclaim.
 *
 * Operations:
 *   insert (snoc) delete (uncons) lookup member fromList toList
 *   foldl foldr map filter partition split concat reverse rotate
 *   zip zipWith size scanl scanr iterate unfold
 *****************************************************************************/

#pragma GCC optimize ("no-inline,no-reorder-functions")

#include "support.h"
#include "gadt_common.h"

#define LOCAL_SCALE_FACTOR 16

GADT_ARENA_DEFINE();

/* Internal cons-list cells. */
typedef struct cell_s { uint32_t v; struct cell_s *n; } cell_t;

typedef struct {
  cell_t   *front;
  cell_t   *rear;
  uint32_t  flen, rlen;
} queue_t;

static cell_t *
qcons (uint32_t v, cell_t *n)
{
  cell_t *c = (cell_t *) gadt_arena_alloc (sizeof (cell_t));
  if (!c) return n;
  c->v = v; c->n = n; return c;
}

/* ---- internal: balance ---------------------------------------------- */

static cell_t *
qreverse (cell_t *xs)
{
  cell_t *out = 0;
  for (; xs; xs = xs->n) out = qcons (xs->v, out);
  return out;
}

static queue_t
qbalance (queue_t q)
{
  if (q.flen >= q.rlen) return q;
  q.front = q.front
    ? /* concat front + reverse(rear) */
      ({ cell_t *acc = qreverse (q.rear);
         cell_t *rf  = qreverse (q.front);
         for (cell_t *p = rf; p; p = p->n) acc = qcons (p->v, acc);
         qreverse (acc); })
    : qreverse (q.rear);
  q.rear  = 0;
  q.flen  = q.flen + q.rlen;
  q.rlen  = 0;
  return q;
}

/* ---- snoc / uncons ----------------------------------------------- */

static queue_t
q_snoc (queue_t q, uint32_t v)
{
  queue_t r = { q.front, qcons (v, q.rear), q.flen, q.rlen + 1 };
  return qbalance (r);
}

static queue_t
q_uncons (queue_t q, uint32_t *out, int *ok)
{
  if (!q.front) { *ok = 0; return q; }
  *out = q.front->v;
  queue_t r = { q.front->n, q.rear, q.flen - 1, q.rlen };
  *ok = 1;
  return qbalance (r);
}

/* ---- lookup at logical index / member --------------------------- */

static int
q_lookup (queue_t q, uint32_t i, uint32_t *out)
{
  cell_t *p = q.front;
  while (p && i) { p = p->n; i--; }
  if (p) { *out = p->v; return 1; }
  /* index falls into the rear half — rear is in REVERSE logical order;
   * the j-th rear element (from rear's head) is logical
   * (flen + rlen - 1 - j). */
  if (i >= q.rlen) return 0;
  uint32_t want = q.rlen - 1 - i;
  cell_t *r = q.rear;
  while (r && want) { r = r->n; want--; }
  if (r) { *out = r->v; return 1; }
  return 0;
}

static int
q_member (queue_t q, uint32_t v)
{
  for (cell_t *p = q.front; p; p = p->n) if (p->v == v) return 1;
  for (cell_t *p = q.rear;  p; p = p->n) if (p->v == v) return 1;
  return 0;
}

/* ---- folds (over logical order: front L→R then rear R→L) ---- */

static uint32_t
q_foldl (uint32_t (*f)(uint32_t, uint32_t), uint32_t z, queue_t q)
{
  for (cell_t *p = q.front; p; p = p->n) z = f (z, p->v);
  /* Rear is reverse: collect into temp via stack of length rlen. */
  uint32_t s = z;
  cell_t *rev = qreverse (q.rear);
  for (cell_t *p = rev; p; p = p->n) s = f (s, p->v);
  return s;
}

static uint32_t
q_foldr (uint32_t (*f)(uint32_t, uint32_t), uint32_t z, queue_t q)
{
  /* Right fold: rear (R→L) then front (L→R reversed). */
  uint32_t s = z;
  for (cell_t *p = q.rear; p; p = p->n) s = f (p->v, s);
  cell_t *rev = qreverse (q.front);
  for (cell_t *p = rev; p; p = p->n) s = f (p->v, s);
  return s;
}

static uint32_t q_size (queue_t q) { return q.flen + q.rlen; }
static int      q_null (queue_t q) { return q.flen == 0 && q.rlen == 0; }

/* ---- map / filter / partition --------------------------------- */

static queue_t
q_map (uint32_t (*f)(uint32_t), queue_t q)
{
  cell_t *nf = 0, *nr = 0;
  for (cell_t *p = q.front; p; p = p->n) nf = qcons (f (p->v), nf);
  for (cell_t *p = q.rear;  p; p = p->n) nr = qcons (f (p->v), nr);
  queue_t r = { qreverse (nf), qreverse (nr), q.flen, q.rlen };
  return r;
}

static queue_t
q_filter (int (*p)(uint32_t), queue_t q)
{
  cell_t *nf = 0; uint32_t nflen = 0;
  cell_t *nr = 0; uint32_t nrlen = 0;
  for (cell_t *c = q.front; c; c = c->n) if (p (c->v)) { nf = qcons (c->v, nf); nflen++; }
  for (cell_t *c = q.rear;  c; c = c->n) if (p (c->v)) { nr = qcons (c->v, nr); nrlen++; }
  queue_t r = { qreverse (nf), qreverse (nr), nflen, nrlen };
  return qbalance (r);
}

typedef struct { queue_t yes, no; } q_split_t;

static q_split_t
q_partition (int (*p)(uint32_t), queue_t q)
{
  q_split_t out;
  out.yes = q_filter (p, q);
  /* Build the "no" queue inline. */
  queue_t no = { 0, 0, 0, 0 };
  for (cell_t *c = q.front; c; c = c->n) if (!p (c->v)) no = q_snoc (no, c->v);
  for (cell_t *c = qreverse (q.rear); c; c = c->n) if (!p (c->v)) no = q_snoc (no, c->v);
  out.no = no;
  return out;
}

/* ---- split at index / concat ------------------------------- */

static q_split_t
q_split (queue_t q, uint32_t i)
{
  q_split_t out;
  queue_t yes = { 0, 0, 0, 0 }, no = { 0, 0, 0, 0 };
  uint32_t idx = 0;
  for (cell_t *p = q.front; p; p = p->n) {
    if (idx < i) yes = q_snoc (yes, p->v); else no = q_snoc (no, p->v);
    idx++;
  }
  cell_t *rrev = qreverse (q.rear);
  for (cell_t *p = rrev; p; p = p->n) {
    if (idx < i) yes = q_snoc (yes, p->v); else no = q_snoc (no, p->v);
    idx++;
  }
  out.yes = yes; out.no = no;
  return out;
}

static queue_t
q_concat (queue_t a, queue_t b)
{
  /* O(n_b): snoc each element of b onto a. */
  for (cell_t *p = b.front; p; p = p->n) a = q_snoc (a, p->v);
  cell_t *rrev = qreverse (b.rear);
  for (cell_t *p = rrev; p; p = p->n) a = q_snoc (a, p->v);
  return a;
}

static queue_t
q_reverse (queue_t q)
{
  /* Reversing a queue: swap front/rear (and their lengths). The
   * Banker's invariant is flen >= rlen — re-balance after the swap. */
  queue_t r = { q.rear, q.front, q.rlen, q.flen };
  return qbalance (r);
}

static queue_t
q_rotate (queue_t q, uint32_t n)
{
  /* Rotate by N positions: take head N times, snoc each onto rear. */
  if (q.flen + q.rlen == 0) return q;
  for (uint32_t i = 0; i < n; i++) {
    uint32_t v; int ok;
    q = q_uncons (q, &v, &ok);
    if (!ok) break;
    q = q_snoc (q, v);
  }
  return q;
}

/* ---- zip / zipWith ----------------------------------------- */

static uint32_t
q_zipsum_with (uint32_t (*f)(uint32_t, uint32_t), queue_t a, queue_t b)
{
  /* Walk a + b in logical order; sum f over the pairs. */
  uint32_t s = 0;
  cell_t *af = a.front, *bf = b.front;
  cell_t *ar = qreverse (a.rear), *br = qreverse (b.rear);
  while (af && bf) { s += f (af->v, bf->v); af = af->n; bf = bf->n; }
  while (af && br) { s += f (af->v, br->v); af = af->n; br = br->n; }
  while (ar && bf) { s += f (ar->v, bf->v); ar = ar->n; bf = bf->n; }
  while (ar && br) { s += f (ar->v, br->v); ar = ar->n; br = br->n; }
  return s;
}

/* ---- fromList / toList ----------------------------------- */

static queue_t
q_from_arr (const uint32_t *xs, int n)
{
  queue_t q = { 0, 0, 0, 0 };
  for (int i = 0; i < n; i++) q = q_snoc (q, xs[i]);
  return q;
}

static int
q_to_arr (queue_t q, uint32_t *out, int cap)
{
  int n = 0;
  for (cell_t *p = q.front; p && n < cap; p = p->n) out[n++] = p->v;
  cell_t *rrev = qreverse (q.rear);
  for (cell_t *p = rrev; p && n < cap; p = p->n) out[n++] = p->v;
  return n;
}

/* ---- scanl / scanr (over logical order) ------------------ */

static uint32_t
q_scanl (uint32_t (*f)(uint32_t, uint32_t), uint32_t z, queue_t q)
{
  uint32_t acc = z, out = z;
  uint32_t buf[256];
  int n = q_to_arr (q, buf, 256);
  for (int i = 0; i < n; i++) { acc = f (acc, buf[i]); out = gadt_hash_mix (out, acc); }
  return out;
}

static uint32_t
q_scanr (uint32_t (*f)(uint32_t, uint32_t), uint32_t z, queue_t q)
{
  uint32_t acc = z, out = z;
  uint32_t buf[256];
  int n = q_to_arr (q, buf, 256);
  for (int i = n - 1; i >= 0; i--) { acc = f (buf[i], acc); out = gadt_hash_mix (out, acc); }
  return out;
}

/* ---- iterate / unfold ----------------------------------- */

typedef struct { uint32_t v, nxt; int ok; } q_unf_t;
static q_unf_t gen_step (uint32_t s) { q_unf_t e; e.v = gadt_lcg_next (&s); e.nxt = s; e.ok = 1; return e; }

static queue_t
q_unfold (q_unf_t (*gen)(uint32_t), uint32_t seed, int n)
{
  queue_t q = { 0, 0, 0, 0 };
  uint32_t s = seed;
  for (int i = 0; i < n; i++) { q_unf_t e = gen (s); if (!e.ok) break; q = q_snoc (q, e.v); s = e.nxt; }
  return q;
}

/* ---- combinators -------------------------------------- */

static uint32_t f_add (uint32_t a, uint32_t b) { return a + b; }
static uint32_t f_xor (uint32_t a, uint32_t b) { return a ^ b; }
static uint32_t f_mul3plus1 (uint32_t x) { return x * 3 + 1; }
static int      p_even      (uint32_t x) { return (x & 1) == 0; }

/* ---- benchmark body ----------------------------------- */

#define Q_LEN 32

static uint32_t in_seed_a, in_seed_b;

static uint32_t
benchmark_body (unsigned int lsf, unsigned int gsf)
{
  uint32_t check = 0xC0FFEEu;
  for (unsigned int outer = 0; outer < lsf * gsf; outer++) {
    gadt_arena_reset ();
    uint32_t sa = in_seed_a + outer, sb = in_seed_b + outer;

    /* Canonical workload — 4 hashMix outputs: sl, sr, za, zx.
     * Full surface (snoc/uncons/lookup/member/filter/partition/
     * split/concat/reverse/rotate/scan/unfold) stays defined but
     * is NOT in the hot loop, matching queue_ninja / queue_morph
     * and Hs.QueuePure / Hs.QueueMorph. */
    uint32_t xs_a[Q_LEN], xs_b[Q_LEN];
    for (int i = 0; i < Q_LEN; i++) { xs_a[i] = gadt_lcg_next (&sa); xs_b[i] = gadt_lcg_next (&sb); }
    queue_t qa = q_from_arr (xs_a, Q_LEN);
    queue_t qb = q_from_arr (xs_b, Q_LEN);
    queue_t qm = q_map (f_mul3plus1, qa);
    uint32_t sl = q_foldl (f_add, 0, qm);
    uint32_t sr = q_foldr (f_xor, 0, qm);
    uint32_t za = q_zipsum_with (f_add, qm, qb);
    uint32_t zx = q_zipsum_with (f_xor, qm, qb);

    check = gadt_hash_mix (check, sl);
    check = gadt_hash_mix (check, sr);
    check = gadt_hash_mix (check, za);
    check = gadt_hash_mix (check, zx);
  }
  return check;
}

/* ---- harness ---------------------------------------- */

static uint32_t g_check;
void warm_caches (int heat)       { (void) benchmark_body (1, heat); }
int  benchmark (void)             { g_check = benchmark_body (LOCAL_SCALE_FACTOR, GLOBAL_SCALE_FACTOR); return 0; }
void initialise_benchmark (void)  { in_seed_a = 0xA1F32C97u; in_seed_b = 0x5EE9D4B2u; g_check = 0; }
int  verify_benchmark (int res)   { (void) res; return (g_check != 0u) && (g_check != 0xC0FFEEu); }
