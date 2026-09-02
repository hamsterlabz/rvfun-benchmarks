/* parser.c -- nofib real/parser in C, with the dynamic data structures the
 * job actually needs.
 *
 * Same application: lex a 992-byte Haskell source, parse it into a module AST,
 * render that AST the way a derived Show would, and hash the rendering.
 * Same result: the hash of the rendered string.
 *
 * The Haskell gets its AST from constructors and its rendering from `deriving
 * Show`. In C both have to be built: a tagged union per node, malloc'd; a
 * grown output buffer; and explicit parenthesisation, because derived Show
 * parenthesises a constructor application exactly when it sits as an argument
 * of another.
 *
 * As in clausify.c the arena is dropped whole rather than freed node by node.
 * The AST is a DAG only in the sense that strings are shared; tracking
 * ownership would measure the memory discipline, not the parse.
 */

#include <stdlib.h>
#include "support.h"

#define LOCAL_SCALE_FACTOR 1

#define HEAP_SIZE (256 * 1024)
static char heap[HEAP_SIZE] __attribute__ ((aligned (8)));

/* ---- the input -------------------------------------------------------- */

static const char *source =
"\n--==========================================================--\n"
"--=== The parser.                                        ===--\n"
"--===                                          Parser.hs ===--\n"
"--==========================================================--\n\n"
"module Parser where\n\n"
"{- FIX THESE UP -}\n"
"--utLookupDef env k def\n"
"--   = head ( [ vv | (kk,vv) <- env, kk == k] ++ [def] )\n"
"panic = error\n"
"{- END FIXUPS -}\n\n"
"--paLiteral :: Parser Literal\n"
"paLiteral\n"
"   = pgAlts\n"
"     [\n"
"        pgApply (LiteralInt.leStringToInt) (pgItem Lintlit),\n"
"        pgApply (LiteralChar.head)         (pgItem Lcharlit),\n"
"        pgApply LiteralString              (pgItem Lstringlit)\n"
"     ]\n\n"
"paExpr\n"
"   = pgAlts\n"
"     [\n"
"        paCaseExpr,\n"
"        paLetExpr,\n"
"        paLamExpr,\n"
"        paIfExpr,\n"
"        paUnaryMinusExpr,\n"
"        hsDoExpr []\n"
"     ]\n\n"
"paUnaryMinusExpr\n"
"   = pgThen2\n"
"        (\\minus (_, aexpr, _) ->\n"
"             ExprApp (ExprApp (ExprVar \"-\") (ExprLiteral (LiteralInt 0))) aexpr)\n"
"        paMinus\n"
"        paAExpr\n";

/* ---- lexer ------------------------------------------------------------ */

typedef enum {
  TVARID, TCONID, TVARSYM, TINT, TSTRING, TCHAR,
  TLPAREN, TRPAREN, TLBRACK, TRBRACK, TCOMMA,
  TEQUALS, TLAMBDA, TARROW, TUNDER, TEOF
} TokKind;

typedef struct Tok {
  TokKind kind;
  const char *text;      /* identifier / symbol / literal spelling */
  int len;
  int ival;
  int line, col;
  struct Tok *next;
} Tok;

static char *arena_str (const char *p, int n)
{
  char *s = (char *) malloc_beebs (n + 1);
  int i;
  for (i = 0; i < n; i++) s[i] = p[i];
  s[n] = 0;
  return s;
}

static int is_lower (char c) { return (c >= 'a' && c <= 'z') || c == '_'; }
static int is_upper (char c) { return c >= 'A' && c <= 'Z'; }
static int is_digit (char c) { return c >= '0' && c <= '9'; }
static int is_idch  (char c) { return is_lower (c) || is_upper (c) || is_digit (c) || c == '\''; }
static int is_sym   (char c)
{
  const char *s = "!#$%&*+./<=>?@\\^|-~:";
  int i;
  for (i = 0; s[i]; i++) if (c == s[i]) return 1;
  return 0;
}

static int tok_col;                     /* column of the token being made */

static Tok *tok_new (TokKind k, const char *t, int n, int line)
{
  Tok *x = (Tok *) malloc_beebs (sizeof (Tok));
  x->kind = k; x->text = t ? arena_str (t, n) : 0; x->len = n;
  x->ival = 0; x->line = line; x->col = tok_col; x->next = 0;
  return x;
}

/* laMain: the lexer. Comments (-- to end of line, {- -} blocks) vanish,
   layout is not needed because every binding here starts in column 1. */
static Tok *lex_all (const char *p)
{
  Tok *head = 0, **tail = &head;
  const char *bol = p;                  /* beginning of the current line */
  int line = 1;

  while (*p)
    {
      tok_col = (int) (p - bol) + 1;
      if (*p == '\n') { line++; p++; bol = p; continue; }
      if (*p == ' ' || *p == '\t' || *p == '\r') { p++; continue; }

      if (p[0] == '{' && p[1] == '-')
        {
          p += 2;
          while (*p && !(p[0] == '-' && p[1] == '}'))
            { if (*p == '\n') line++; p++; }
          if (*p) p += 2;
          continue;
        }
      if (p[0] == '-' && p[1] == '-')
        {
          while (*p && *p != '\n') p++;
          continue;
        }

      if (is_lower (*p))
        {
          const char *s = p;
          while (is_idch (*p)) p++;
          if (p - s == 1 && s[0] == '_')
            { *tail = tok_new (TUNDER, 0, 0, line); tail = &(*tail)->next; continue; }
          *tail = tok_new (TVARID, s, (int) (p - s), line);
          tail = &(*tail)->next;
          continue;
        }
      if (is_upper (*p))
        {
          const char *s = p;
          while (is_idch (*p)) p++;
          *tail = tok_new (TCONID, s, (int) (p - s), line);
          tail = &(*tail)->next;
          continue;
        }
      if (is_digit (*p))
        {
          const char *s = p; int v = 0;
          while (is_digit (*p)) { v = v * 10 + (*p - '0'); p++; }
          *tail = tok_new (TINT, s, (int) (p - s), line);
          (*tail)->ival = v; tail = &(*tail)->next;
          continue;
        }
      if (*p == '"')
        {
          const char *s = ++p;
          while (*p && *p != '"') { if (*p == '\\') p++; p++; }
          *tail = tok_new (TSTRING, s, (int) (p - s), line);
          tail = &(*tail)->next;
          if (*p) p++;
          continue;
        }
      if (*p == '\'')
        {
          const char *s = ++p;
          while (*p && *p != '\'') { if (*p == '\\') p++; p++; }
          *tail = tok_new (TCHAR, s, (int) (p - s), line);
          tail = &(*tail)->next;
          if (*p) p++;
          continue;
        }

      switch (*p)
        {
        case '(': *tail = tok_new (TLPAREN, 0, 0, line); tail = &(*tail)->next; p++; continue;
        case ')': *tail = tok_new (TRPAREN, 0, 0, line); tail = &(*tail)->next; p++; continue;
        case '[': *tail = tok_new (TLBRACK, 0, 0, line); tail = &(*tail)->next; p++; continue;
        case ']': *tail = tok_new (TRBRACK, 0, 0, line); tail = &(*tail)->next; p++; continue;
        case ',': *tail = tok_new (TCOMMA,  0, 0, line); tail = &(*tail)->next; p++; continue;
        default: break;
        }

      if (is_sym (*p))
        {
          const char *s = p;
          while (is_sym (*p)) p++;
          {
            int n = (int) (p - s);
            if (n == 1 && s[0] == '=')
              { *tail = tok_new (TEQUALS, 0, 0, line); }
            else if (n == 1 && s[0] == '\\')
              { *tail = tok_new (TLAMBDA, 0, 0, line); }
            else if (n == 2 && s[0] == '-' && s[1] == '>')
              { *tail = tok_new (TARROW, 0, 0, line); }
            else
              { *tail = tok_new (TVARSYM, s, n, line); }
            tail = &(*tail)->next;
          }
          continue;
        }

      p++;                              /* anything else: skip */
    }

  *tail = tok_new (TEOF, 0, 0, 99997);
  return head;
}

/* ---- AST -------------------------------------------------------------- */

typedef enum { E_VAR, E_CON, E_APP, E_LIST, E_LAM, E_LIT } EKind;
typedef enum { L_INT, L_STRING, L_CHAR } LKind;
typedef enum { P_VAR, P_WILD, P_TUPLE } PKind;

typedef struct Pat {
  PKind kind;
  char *name;
  struct PatL *items;                   /* P_TUPLE */
} Pat;

typedef struct PatL { Pat *hd; struct PatL *tl; } PatL;

typedef struct Expr {
  EKind kind;
  char *name;                           /* E_VAR / E_CON */
  struct Expr *l, *r;                   /* E_APP */
  struct ExprL *items;                  /* E_LIST */
  PatL *pats;                           /* E_LAM */
  struct Expr *body;                    /* E_LAM */
  LKind lkind; int ival; char *sval;    /* E_LIT */
} Expr;

typedef struct ExprL { Expr *hd; struct ExprL *tl; } ExprL;

typedef struct Bind { int line; Pat *lhs; Expr *rhs; struct Bind *next; } Bind;

static Expr *e_new (EKind k)
{
  Expr *e = (Expr *) malloc_beebs (sizeof (Expr));
  e->kind = k; e->name = 0; e->l = e->r = e->body = 0;
  e->items = 0; e->pats = 0; e->lkind = L_INT; e->ival = 0; e->sval = 0;
  return e;
}

static Pat *p_new (PKind k)
{
  Pat *p = (Pat *) malloc_beebs (sizeof (Pat));
  p->kind = k; p->name = 0; p->items = 0;
  return p;
}

/* ---- parser ----------------------------------------------------------- */

static Tok *cur;

static int at (TokKind k) { return cur && cur->kind == k; }
static void adv (void)    { if (cur) cur = cur->next; }

static Expr *parse_expr (void);

static Pat *parse_pat (void)
{
  if (at (TUNDER)) { adv (); return p_new (P_WILD); }
  if (at (TVARID))
    {
      Pat *p = p_new (P_VAR);
      p->name = (char *) cur->text;
      adv ();
      return p;
    }
  if (at (TLPAREN))
    {
      PatL *head = 0, **tail = &head;
      adv ();
      while (!at (TRPAREN) && cur)
        {
          Pat *q = parse_pat ();
          *tail = (PatL *) malloc_beebs (sizeof (PatL));
          (*tail)->hd = q; (*tail)->tl = 0; tail = &(*tail)->tl;
          if (at (TCOMMA)) adv ();
        }
      if (at (TRPAREN)) adv ();
      {
        Pat *t = p_new (P_TUPLE);
        t->items = head;
        return t;
      }
    }
  adv ();
  return p_new (P_WILD);
}

/* an atom: var, con, literal, (expr), [exprs], lambda */
static Expr *parse_atom (void)
{
  if (at (TVARID))
    {
      Expr *e = e_new (E_VAR); e->name = (char *) cur->text; adv (); return e;
    }
  if (at (TCONID))
    {
      Expr *e = e_new (E_CON); e->name = (char *) cur->text; adv (); return e;
    }
  if (at (TINT))
    {
      Expr *e = e_new (E_LIT); e->lkind = L_INT; e->ival = cur->ival; adv (); return e;
    }
  if (at (TSTRING))
    {
      Expr *e = e_new (E_LIT); e->lkind = L_STRING; e->sval = (char *) cur->text; adv (); return e;
    }
  if (at (TCHAR))
    {
      Expr *e = e_new (E_LIT); e->lkind = L_CHAR; e->sval = (char *) cur->text; adv (); return e;
    }
  if (at (TLAMBDA))
    {
      Expr *e = e_new (E_LAM);
      PatL *head = 0, **tail = &head;
      adv ();
      while (!at (TARROW) && cur && cur->kind != TEOF)
        {
          Pat *q = parse_pat ();
          *tail = (PatL *) malloc_beebs (sizeof (PatL));
          (*tail)->hd = q; (*tail)->tl = 0; tail = &(*tail)->tl;
        }
      if (at (TARROW)) adv ();
      e->pats = head;
      e->body = parse_expr ();
      return e;
    }
  if (at (TLPAREN))
    {
      Expr *e;
      adv ();
      e = parse_expr ();
      if (at (TRPAREN)) adv ();
      return e;
    }
  if (at (TLBRACK))
    {
      Expr *e = e_new (E_LIST);
      ExprL *head = 0, **tail = &head;
      adv ();
      while (!at (TRBRACK) && cur && cur->kind != TEOF)
        {
          Expr *x = parse_expr ();
          *tail = (ExprL *) malloc_beebs (sizeof (ExprL));
          (*tail)->hd = x; (*tail)->tl = 0; tail = &(*tail)->tl;
          if (at (TCOMMA)) adv ();
        }
      if (at (TRBRACK)) adv ();
      e->items = head;
      return e;
    }
  return 0;
}

/* THE OFFSIDE RULE, in the only form this input needs: every top-level
 * binding starts in column 1, so a token there closes whatever expression is
 * open. Without it an application runs straight through `panic = error` into
 * the next binding's name and swallows it. */
static int at_toplevel (void)
{
  return cur && cur->col == 1 && cur->kind != TEOF;
}

static int starts_atom (void)
{
  if (at_toplevel ()) return 0;
  return at (TVARID) || at (TCONID) || at (TINT) || at (TSTRING)
      || at (TCHAR)  || at (TLPAREN) || at (TLBRACK) || at (TLAMBDA);
}

/* application: left associative, tighter than any operator */
static Expr *parse_app (void)
{
  Expr *f = parse_atom ();
  if (!f) return 0;
  while (starts_atom ())
    {
      Expr *x = parse_atom ();
      Expr *a;
      if (!x) break;
      a = e_new (E_APP); a->l = f; a->r = x;
      f = a;
    }
  return f;
}

/* operators: the only one this input uses is `.`, and derived Show renders
   it as an ordinary application of ExprVar "." to both sides */
static Expr *parse_expr (void)
{
  Expr *l = parse_app ();
  while (at (TVARSYM) && !at_toplevel ())
    {
      Expr *op = e_new (E_VAR);
      Expr *r, *a1, *a2;
      op->name = (char *) cur->text;
      adv ();
      r = parse_app ();
      a1 = e_new (E_APP); a1->l = op; a1->r = l;
      a2 = e_new (E_APP); a2->l = a1; a2->r = r;
      l = a2;
    }
  return l;
}

/* the module: header, then a run of `name = expr` bindings */
static Bind *parse_module (Tok *ts, char **modname)
{
  Bind *head = 0, **tail = &head;
  cur = ts;

  if (at (TVARID) && cur->text[0] == 'm')      /* module */
    {
      adv ();
      if (at (TCONID)) { *modname = (char *) cur->text; adv (); }
      if (at (TVARID)) adv ();                 /* where */
    }

  while (cur && cur->kind != TEOF)
    {
      if (at (TVARID) && cur->next && cur->next->kind == TEQUALS)
        {
          Bind *b = (Bind *) malloc_beebs (sizeof (Bind));
          Pat *p = p_new (P_VAR);
          p->name = (char *) cur->text;
          b->lhs = p;
          /* the line is the NAME's, not the `=`'s: paLiteral sits on 16 with
             its `=` on 17, and the reference rendering says 16 */
          b->line = cur->line;
          adv ();                        /* the name */
          adv ();                        /* the `=` */
          b->rhs = parse_expr ();
          b->next = 0;
          *tail = b; tail = &b->next;
          continue;
        }
      adv ();
    }
  return head;
}

/* ---- derived-Show rendering ------------------------------------------- */

static char *outbuf;
static int outlen, outcap;

static void emit_ch (char c)
{
  if (outlen + 1 >= outcap) return;      /* buffer is sized for this input */
  outbuf[outlen++] = c;
}

static void emit (const char *s) { while (*s) emit_ch (*s++); }

static void emit_int (int v)
{
  char tmp[12]; int n = 0;
  if (v == 0) { emit_ch ('0'); return; }
  if (v < 0) { emit_ch ('-'); v = -v; }
  while (v) { tmp[n++] = (char) ('0' + v % 10); v /= 10; }
  while (n) emit_ch (tmp[--n]);
}

static void emit_str_lit (const char *s)
{
  emit_ch ('"');
  while (*s)
    {
      if (*s == '"' || *s == '\\') emit_ch ('\\');
      emit_ch (*s++);
    }
  emit_ch ('"');
}

static void show_pat (Pat *p, int prec);
static void show_expr (Expr *e, int prec);

static void show_patlist (PatL *ps)
{
  emit_ch ('[');
  while (ps)
    {
      show_pat (ps->hd, 0);
      ps = ps->tl;
      if (ps) emit_ch (',');
    }
  emit_ch (']');
}

/* prec 11 means "I am an argument": derived Show parenthesises a constructor
   application there, and only there. */
static void show_pat (Pat *p, int prec)
{
  switch (p->kind)
    {
    case P_WILD: emit ("PatWild"); return;
    case P_VAR:
      if (prec >= 11) emit_ch ('(');
      emit ("PatVar "); emit_str_lit (p->name);
      if (prec >= 11) emit_ch (')');
      return;
    default:
      if (prec >= 11) emit_ch ('(');
      emit ("PatTuple "); show_patlist (p->items);
      if (prec >= 11) emit_ch (')');
      return;
    }
}

static void show_lit (Expr *e, int prec)
{
  if (prec >= 11) emit_ch ('(');
  switch (e->lkind)
    {
    case L_INT:    emit ("LiteralInt ");    emit_int (e->ival); break;
    case L_STRING: emit ("LiteralString "); emit_str_lit (e->sval); break;
    default:       emit ("LiteralChar ");   emit_ch ('\''); emit (e->sval); emit_ch ('\''); break;
    }
  if (prec >= 11) emit_ch (')');
}

static void show_expr (Expr *e, int prec)
{
  switch (e->kind)
    {
    case E_VAR:
      if (prec >= 11) emit_ch ('(');
      emit ("ExprVar "); emit_str_lit (e->name);
      if (prec >= 11) emit_ch (')');
      return;
    case E_CON:
      if (prec >= 11) emit_ch ('(');
      emit ("ExprCon "); emit_str_lit (e->name);
      if (prec >= 11) emit_ch (')');
      return;
    case E_APP:
      if (prec >= 11) emit_ch ('(');
      emit ("ExprApp ");
      show_expr (e->l, 11); emit_ch (' '); show_expr (e->r, 11);
      if (prec >= 11) emit_ch (')');
      return;
    case E_LIST:
      if (prec >= 11) emit_ch ('(');
      emit ("ExprList [");
      {
        ExprL *x = e->items;
        while (x) { show_expr (x->hd, 0); x = x->tl; if (x) emit_ch (','); }
      }
      emit_ch (']');
      if (prec >= 11) emit_ch (')');
      return;
    case E_LAM:
      if (prec >= 11) emit_ch ('(');
      emit ("ExprLam "); show_patlist (e->pats); emit_ch (' ');
      show_expr (e->body, 11);
      if (prec >= 11) emit_ch (')');
      return;
    default:
      if (prec >= 11) emit_ch ('(');
      emit ("ExprLiteral "); show_lit (e, 11);
      if (prec >= 11) emit_ch (')');
      return;
    }
}

static void show_module (const char *name, Bind *bs)
{
  emit ("MkModule "); emit_str_lit (name); emit (" [");
  while (bs)
    {
      emit ("MkTopV (MkValBind "); emit_int (bs->line);
      emit (" (LhsPat "); show_pat (bs->lhs, 11); emit (") ");
      show_expr (bs->rhs, 11);
      emit (")");
      bs = bs->next;
      if (bs) emit_ch (',');
    }
  emit_ch (']');
}

/* ---- showx + hash ----------------------------------------------------- */

#define OUTCAP 8192
static char outstore[OUTCAP];

static int env_size = 21;               /* length hsPrecTable */

int render_len;                          /* cross-check against the Haskell */

static int benchmark_body (unsigned int lsf, unsigned int gsf)
{
  int h = 0;
  unsigned int i, j;

  for (i = 0; i < lsf; i++)
    for (j = 0; j < gsf; j++)
      {
        Tok *ts;
        Bind *bs;
        char *modname = (char *) "Main";
        int n;

        init_heap_beebs ((void *) heap, HEAP_SIZE);
        outbuf = outstore; outlen = 0; outcap = OUTCAP;

        ts = lex_all (source);
        bs = parse_module (ts, &modname);

        /* showx (POk env toks result) */
        emit ("\n\nSucceeded, with:\n   Size env = ");
        emit_int (env_size);
        emit ("\n   Next token = (99997,99997,Leof,\"\")");
        emit ("\n\n   Result = ");
        show_module ("Main", bs);
        emit ("\n\n");

        render_len = outlen;

        h = 0;
        for (n = 0; n < outlen; n++)
          h = (int) ((unsigned int) outbuf[n] + (unsigned int) h * 31u);
      }

  return h;
}

void warm_caches (int heat) { (void) benchmark_body (1, heat); }

int benchmark (void) { return benchmark_body (LOCAL_SCALE_FACTOR, GLOBAL_SCALE_FACTOR); }

void initialise_benchmark (void) { }

/* the Haskell renders 1432 characters; hash of that string is the answer */
int verify_benchmark (int r) { return render_len == 1432 && r == -557516925; }
