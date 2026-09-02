/* clausify.c -- nofib/flite Clausify in C, with the dynamic data structures
 * the algorithm actually needs.
 *
 * Same application: put a propositional formula into clausal form --
 * negin (push negations in), disin (distribute disjunction over conjunction),
 * split, collect literals into (positive, negative) clauses, drop tautologies,
 * remove duplicate clauses. Same result: display sums every literal.
 *
 * This is the counterpart to porting Embench to Haskell. There the workload
 * was shaped for C; here it is shaped for a graph reducer, and C has to build
 * what Haskell gets from its constructors: a malloc'd tagged union for the
 * term, singly-linked lists for the clauses, structural equality by traversal.
 *
 * Memory is deliberately NOT freed. The Haskell relies on a collector, and the
 * rewrites share subterms; tracking ownership through disin/din/din2 would
 * mean either refcounts or a copy at every share, and either choice would be
 * measuring the memory discipline rather than the algorithm. An arena that is
 * dropped whole is what a C programmer would actually write here.
 */

#include <stdlib.h>
#include "support.h"

#define LOCAL_SCALE_FACTOR 1

/* ---- terms ------------------------------------------------------------ */

typedef enum { CON, DIS, NEG, SYM } Tag;

typedef struct Expr {
  Tag tag;
  struct Expr *l, *r;   /* CON/DIS: both; NEG: l */
  int sym;              /* SYM */
} Expr;

static Expr *alloc_expr (void)
{
  return (Expr *) malloc_beebs (sizeof (Expr));
}

static Expr *mkbin (Tag t, Expr *a, Expr *b)
{
  Expr *e = alloc_expr (); e->tag = t; e->l = a; e->r = b; e->sym = 0; return e;
}

static Expr *mkneg (Expr *a)
{
  Expr *e = alloc_expr (); e->tag = NEG; e->l = a; e->r = 0; e->sym = 0; return e;
}

static Expr *mksym (int s)
{
  Expr *e = alloc_expr (); e->tag = SYM; e->l = e->r = 0; e->sym = s; return e;
}

/* ---- literal lists, kept sorted, duplicates retained ------------------- */

typedef struct IL { int hd; struct IL *tl; } IL;

static IL *cons_i (int x, IL *t)
{
  IL *c = (IL *) malloc_beebs (sizeof (IL)); c->hd = x; c->tl = t; return c;
}

/* insert x [] = [x]; insert x (y:ys) = x<=y ? x:y:ys : y : insert x ys */
static IL *insert_i (int x, IL *ys)
{
  if (ys == 0) return cons_i (x, 0);
  if (x <= ys->hd) return cons_i (x, ys);
  return cons_i (ys->hd, insert_i (x, ys->tl));
}

static int contains_i (IL *xs, int y)
{
  for (; xs; xs = xs->tl) if (xs->hd == y) return 1;
  return 0;
}

/* ---- clauses ---------------------------------------------------------- */

typedef struct Cl { IL *c, *a; struct Cl *next; } Cl;

static Cl *cons_cl (IL *c, IL *a, Cl *next)
{
  Cl *k = (Cl *) malloc_beebs (sizeof (Cl));
  k->c = c; k->a = a; k->next = next; return k;
}

/* ---- expression lists ------------------------------------------------- */

typedef struct EL { Expr *hd; struct EL *tl; } EL;

static EL *cons_e (Expr *x, EL *t)
{
  EL *c = (EL *) malloc_beebs (sizeof (EL)); c->hd = x; c->tl = t; return c;
}

/* ---- the rewrites ----------------------------------------------------- */

static Expr *negin (Expr *p);

/* negin (Neg e) */
static Expr *negin_neg (Expr *e)
{
  switch (e->tag)
    {
    case CON: return mkbin (DIS, negin_neg (e->l), negin_neg (e->r));
    case DIS: return mkbin (CON, negin_neg (e->l), negin_neg (e->r));
    case NEG: return negin (e->l);
    default:  return mkneg (mksym (e->sym));
    }
}

static Expr *negin (Expr *p)
{
  switch (p->tag)
    {
    case NEG: return negin_neg (p->l);
    case DIS: return mkbin (DIS, negin (p->l), negin (p->r));
    case CON: return mkbin (CON, negin (p->l), negin (p->r));
    default:  return mksym (p->sym);
    }
}

static Expr *din (Expr *p, Expr *r);

static Expr *din2 (Expr *p, Expr *q)
{
  switch (q->tag)
    {
    case CON: return mkbin (CON, din (p, q->l), din (p, q->r));
    case DIS: return mkbin (DIS, p, mkbin (DIS, q->l, q->r));
    default:  return mkbin (DIS, p, q);          /* Neg or Sym */
    }
}

static Expr *din (Expr *p, Expr *r)
{
  if (p->tag == CON) return mkbin (CON, din (p->l, r), din (p->r, r));
  return din2 (p, r);
}

static Expr *disin (Expr *p)
{
  switch (p->tag)
    {
    case CON: return mkbin (CON, disin (p->l), disin (p->r));
    case DIS: return din (disin (p->l), disin (p->r));
    default:  return p;                          /* Sym or Neg, kept as-is */
    }
}

/* spl a (Con p q) = spl (spl a p) q; otherwise the term is consed on */
static EL *spl (EL *a, Expr *p)
{
  if (p->tag == CON) return spl (spl (a, p->l), p->r);
  return cons_e (p, a);
}

/* clause (c,a) walks a disjunction collecting literals */
static void clause_walk (Expr *p, IL **c, IL **a)
{
  switch (p->tag)
    {
    case DIS: clause_walk (p->l, c, a); clause_walk (p->r, c, a); return;
    case SYM: *c = insert_i (p->sym, *c); return;
    case NEG:
      if (p->l->tag == SYM) { *a = insert_i (p->l->sym, *a); return; }
      /* the Haskell falls through to ([],[]) here */
      *c = 0; *a = 0; return;
    default:  *c = 0; *a = 0; return;
    }
}

/* notTaut: no literal appears both positively and negatively */
static int not_taut (Cl *k)
{
  IL *y;
  for (y = k->a; y; y = y->tl) if (contains_i (k->c, y->hd)) return 0;
  return 1;
}

static int eq_il (IL *x, IL *y)
{
  while (x && y) { if (x->hd != y->hd) return 0; x = x->tl; y = y->tl; }
  return x == 0 && y == 0;
}

static int eq_clause (Cl *x, Cl *y)
{
  return eq_il (x->c, y->c) && eq_il (x->a, y->a);
}

/* uniq: foldr over the list, dropping a clause already seen to its left */
static int seen_before (Cl *head, Cl *stop, Cl *k)
{
  Cl *p;
  for (p = head; p && p != stop; p = p->next) if (eq_clause (p, k)) return 1;
  return 0;
}

/* ---- the formula ------------------------------------------------------ */

static Expr *eqv (Expr *a, Expr *b)
{
  return mkbin (CON, mkbin (DIS, mkneg (a), b), mkbin (DIS, mkneg (b), a));
}

/* display $ clausify $ foldr Con (Sym 0) $ replicate 2 p
   where p = eqv (eqv a (eqv a a)) (eqv a (eqv a a)),  a = Sym 0 */
static Expr *formula (void)
{
  Expr *f = mksym (0);                    /* the foldr seed */
  int i;
  for (i = 0; i < 2; i++)
    {
      Expr *a1 = mksym (0), *a2 = mksym (0), *a3 = mksym (0);
      Expr *a4 = mksym (0), *a5 = mksym (0), *a6 = mksym (0);
      Expr *p = eqv (eqv (a1, eqv (a2, a3)), eqv (a4, eqv (a5, a6)));
      f = mkbin (CON, p, f);
    }
  return f;
}

/* The rewrites allocate and share; nothing is freed within a pass, so the
 * arena is reset each iteration instead -- see the note at the top. */
#define HEAP_SIZE (512 * 1024)
static char heap[HEAP_SIZE] __attribute__ ((aligned (8)));

int clause_count, literal_count;        /* structural cross-check vs Haskell */

static int benchmark_body (unsigned int lsf, unsigned int gsf)
{
  int total = 0;
  unsigned int i, j;

  for (i = 0; i < lsf; i++)
    for (j = 0; j < gsf; j++)
      {
        Cl *cls = 0, **tail = &cls;
        EL *terms;
        Cl *k;

        total = 0; clause_count = 0; literal_count = 0;
        init_heap_beebs ((void *) heap, HEAP_SIZE);

        terms = spl (0, disin (negin (formula ())));

        for (; terms; terms = terms->tl)
          {
            IL *c = 0, *a = 0;
            clause_walk (terms->hd, &c, &a);
            *tail = cons_cl (c, a, 0);
            tail = &(*tail)->next;
          }

        /* nonTaut, then uniq, then display */
        for (k = cls; k; k = k->next)
          {
            IL *y;
            if (!not_taut (k)) continue;
            if (seen_before (cls, k, k)) continue;
            clause_count++;
            for (y = k->c; y; y = y->tl) { total += y->hd; literal_count++; }
            for (y = k->a; y; y = y->tl) { total += y->hd; literal_count++; }
          }
      }

  return total;
}

void warm_caches (int heat) { (void) benchmark_body (1, heat); }

int benchmark (void) { return benchmark_body (LOCAL_SCALE_FACTOR, GLOBAL_SCALE_FACTOR); }

void initialise_benchmark (void) { }

int verify_benchmark (int r) { return r == 0; }
