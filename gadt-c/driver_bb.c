/* driver_bb.c - blackbird bare-metal harness for one gadt-c benchmark.
 *
 * #includes the benchmark .c (path via -DBENCH_C="...") so its static
 * initialise_benchmark / benchmark / verify_benchmark / g_check are visible,
 * runs the embench-contract sequence once, and self-reports: a parseable
 * GADT RESULT line plus the shared full-PMC breakdown (bb_perf.h), same
 * format as Dhrystone. Terminates with the standard bare-runtime ecall park.
 */
#include <stdint.h>
#include "bb_perf.h"

int  printf (const char *fmt, ...);
void bb_flush (void);

#ifndef NITERS
#define NITERS 32
#endif
#ifndef GLOBAL_SCALE_FACTOR
#define GLOBAL_SCALE_FACTOR NITERS
#endif
#ifndef BENCH_NAME
#define BENCH_NAME "gadt"
#endif

#include BENCH_C

static bb_perf_t pmc_s, pmc_e;

int
main (void)
{
  initialise_benchmark ();
  __asm__ __volatile__ ("" ::: "memory");
  bb_perf_snap (&pmc_s);
  __asm__ __volatile__ ("" ::: "memory");
  /* EXACTLY NITERS outer iterations (lsf=1), identical to the Haskell
   * `bench NITERS` (same seeds per outer). benchmark() would multiply by
   * LOCAL_SCALE_FACTOR and break the load match. */
  g_check = benchmark_body (1, NITERS);
  __asm__ __volatile__ ("" ::: "memory");
  bb_perf_snap (&pmc_e);
  __asm__ __volatile__ ("" ::: "memory");
  int ok = verify_benchmark (0);
  printf ("GADT RESULT name=%s status=%s check=0x%x cycles=%d\n",
          BENCH_NAME, ok ? "PASS" : "FAIL", (unsigned) g_check,
          (int) (pmc_e.v[0] - pmc_s.v[0]));
  bb_perf_report (&pmc_s, &pmc_e);
  bb_flush ();
  __asm__ volatile ("ecall");
  for (;;) { }
}
