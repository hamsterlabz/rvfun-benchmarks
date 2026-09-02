/******************************************************************************
 * gadt_common.h — shared utilities for the rv0 GADT benchmark suite.
 *
 * The suite characterises rv0 over abstract data structures (Tuples,
 * Lists, Binary Trees, Quadtrees, Rose Trees) by running six core
 * operations on each: insertion, deletion, foldr/foldl, map, zip,
 * zipWith.
 *
 * Two source variants per data structure:
 *   <ds>_pure.c   — functional-C: every operation returns a NEW
 *                   structure built from its inputs, no in-place
 *                   mutation, no globals. Stresses the allocator
 *                   and the cache.
 *   <ds>_ninja.c  — highly-tuned C: in-place updates, register-
 *                   pinned hot paths, manual unrolling where it
 *                   matters. Matches the "kernel/runtime" idiom.
 *
 * Both variants reuse the embench Embench-IoT runner shape:
 *   initialise_benchmark()  — one-shot setup
 *   warm_caches(heat)       — cache prime
 *   benchmark()             — workload entry; returns #errors
 *   verify_benchmark(r)     — return 1 iff PASS
 *
 * Memory comes from a tiny per-translation-unit bump arena. No
 * libc malloc on rv0 freestanding; arenas keep allocation
 * deterministic and lockstep-friendly.
 *****************************************************************************/

#ifndef GADT_COMMON_H
#define GADT_COMMON_H

#include <stdint.h>
#include <stddef.h>

/* ----- Bump arena allocator ------------------------------------------------
 *
 * Each TU declares `static uint8_t arena_buf[ARENA_SIZE];` and a
 * single cursor `g_arena_off`. arena_alloc returns 4-byte-aligned
 * pointers and bumps the cursor; arena_reset rewinds to zero (used
 * between functional iterations to keep peak usage bounded).
 *
 * Ninja variants use the arena too — the "ninja" advantage is in
 * how the data structure is laid out and traversed, not in the
 * allocator. Both variants share the same allocation budget for a
 * fair perf comparison.
 */

#ifndef GADT_ARENA_SIZE
#define GADT_ARENA_SIZE  (32 * 1024)   /* 32 KB per benchmark TU */
#endif

extern uint8_t  gadt_arena_buf[GADT_ARENA_SIZE];
extern uint32_t gadt_arena_off;

static inline void *
gadt_arena_alloc (uint32_t bytes)
{
  uint32_t off = (gadt_arena_off + 3u) & ~3u;   /* 4-byte align */
  if (off + bytes > GADT_ARENA_SIZE) return 0;  /* OOM → caller drops */
  gadt_arena_off = off + bytes;
  return &gadt_arena_buf[off];
}

static inline void
gadt_arena_reset (void)
{
  gadt_arena_off = 0;
}

static inline uint32_t
gadt_arena_used (void)
{
  return gadt_arena_off;
}

/* The .c file that #includes this header must define the buffer +
 * cursor exactly once. The macro keeps every TU's arena local —
 * no cross-bench contamination if two are ever linked together. */
#define GADT_ARENA_DEFINE() \
  uint8_t  gadt_arena_buf[GADT_ARENA_SIZE]; \
  uint32_t gadt_arena_off = 0

/* ----- Tiny linear-congruential generator --------------------------------
 *
 * Used by every benchmark for input generation. Deterministic, so
 * verify_benchmark() can hard-code the expected output checksum.
 * Knuth-style multiplier; trivial code, no stalls.
 */

static inline uint32_t
gadt_lcg_next (uint32_t *s)
{
  *s = (*s) * 1664525u + 1013904223u;
  return *s;
}

/* Coerce an LCG sample into [0, n).  No bias-correction — n is
 * small in the benches so the bias is below the measurement
 * noise floor. */
static inline uint32_t
gadt_lcg_range (uint32_t *s, uint32_t n)
{
  return gadt_lcg_next (s) % (n ? n : 1u);
}

/* ----- Per-DS shared helpers -------------------------------------------- */

/* Plain 32-bit hash mix; used to fold a sequence of values into a
 * single checksum that verify_benchmark() can compare. xorshift32
 * variant — cheap on rv32. */
static inline uint32_t
gadt_hash_mix (uint32_t h, uint32_t v)
{
  h ^= v;
  h *= 0x85ebca6bu;
  h ^= h >> 13;
  h *= 0xc2b2ae35u;
  h ^= h >> 16;
  return h;
}

#endif /* GADT_COMMON_H */
