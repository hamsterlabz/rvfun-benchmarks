/******************************************************************************
 * gadt_morph.h — recursion-scheme conventions for the GADT *_morph suite.
 *
 * The four basic morphisms over an inductive datatype  μF  are:
 *
 *   catamorphism   cata  :: (F a       → a) →   μF       → a
 *   anamorphism    ana   :: (a       → F a) →    a       → μF
 *   hylomorphism   hylo  :: (F b → b)·(a → F a)  →  a    → b
 *   paramorphism   para  :: (F (μF, a) → a) →    μF      → a
 *
 *   cata  consumes a structure to a value:
 *           replaces every constructor of μF with an algebra-step.
 *   ana   produces a structure from a seed:
 *           the coalgebra repeatedly emits one layer of F + sub-seeds.
 *   hylo  is the fusion  cata · ana : neither the intermediate
 *           structure nor any of its nodes is materialised.
 *   para  is cata with access to the SUB-STRUCTURE that produced
 *           each recursive result — useful for ops like "insert at
 *           sorted position", "tails", "drop-while".
 *
 * C is monomorphic, so there is no generic μF.  Each data structure
 * therefore declares its own functor-shaped algebra / coalgebra and
 * provides four functions named uniformly:
 *
 *     <ds>_cata  alg            xs
 *     <ds>_ana            coalg s
 *     <ds>_hylo  alg      coalg s
 *     <ds>_para  para-alg       xs
 *
 * The benefit at the source level is that recursive operations on a
 * structure stop being one-off ad-hoc traversals: they all become
 *   "this is a catamorphism with such-and-such algebra"
 * which makes the family of operations directly comparable across
 * data structures.  The compiled code keeps the recursion — the
 * morphism is a SOURCE-LEVEL abstraction; nothing is fused away
 * unless the compiler decides to inline the algebra calls.  That
 * inlining is exactly what the rv32 optimizer does at -O3 with the
 * pragma-locked translation-unit boundary, so the *_morph variants
 * are expected to track the *_pure variants in cycles within a few
 * percent on benchmarks.  The interesting thing they expose is the
 * structural cost of paying the abstraction at the source level.
 *
 * Conventions used by the *_morph translation units:
 *
 *   ALGEBRA STRUCTS hold one function pointer per constructor of F.
 *     For a binary-tree functor F a = Leaf | Node v a a, that's:
 *       typedef struct {
 *         RESULT (*leaf)(void);
 *         RESULT (*node)(uint32_t v, RESULT lr, RESULT rr);
 *       } bt_alg_t;
 *
 *   COALGEBRA STRUCTS report whether a seed yields a base-case layer
 *     or a one-step-richer layer, plus the projection of value + new
 *     sub-seeds.  Concretely for a list:
 *       typedef struct {
 *         int      (*is_nil)(uint32_t seed);
 *         uint32_t (*head)  (uint32_t seed);
 *         uint32_t (*tail)  (uint32_t seed);
 *       } list_coalg_t;
 *
 *   PARA ALGEBRA STRUCTS receive the sub-structure ALONG WITH the
 *     recursive result, so the algebra can inspect "what's left".
 *
 * Shared utility typedefs follow.  Each *_morph TU includes this
 * header for the documentation contract + these typedefs.
 *****************************************************************************/

#ifndef GADT_MORPH_H
#define GADT_MORPH_H

#include <stdint.h>

/* Binary fold/zip combinator. */
typedef uint32_t (*gadt_bin_fn) (uint32_t, uint32_t);

/* Unary element transform. */
typedef uint32_t (*gadt_un_fn)  (uint32_t);

/* Predicate. */
typedef int      (*gadt_pred_fn)(uint32_t);

/* Generator step: a coalgebra reports a single layer of F. The
 * specific shape is per-functor; this header only fixes the names.
 */

#endif /* GADT_MORPH_H */
