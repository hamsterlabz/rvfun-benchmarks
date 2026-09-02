/* PORT (Double->Float rule, all versions binary32 on the rv32f FPU):
 * double -> float, double literals -> f-suffixed, libm calls ->
 * f-variants.  No other change from the upstream source. */
/*
** The Great Computer Language Shootout
** http://shootout.alioth.debian.org/
** contributed by Mike Pall
**
** compile with:
**   gcc -O3 -fomit-frame-pointer -ffast-math -o partialsums partialsums.c -lm
**   Adding -march=<yourcpu> may help, too.
**   On a P4/K8 or later try adding: --march=<yourcpu> -mfpmath=sse -msse2 
*/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

int main(int argc, char **argv)
{
  int k, n = atoi(argv[1]);
  float sum;

/*
** Yes, I tried using a float as a primary or secondary loop variable.
** But the x86 ABI requires a cleared x87 FPU stack before every call
** (e.g. to sinf()) which nullifies any performance gains.
**
** Combining all loops does not pay off because the x87 FPU has to shuffle
** stack slots and/or runs out of registers. This may not be entirely true
** for SSE2 with fully inlined FPU code (-ffast-math required). Dito for
** other CPUs with a register-based FPU and a sane FP ABI.
**
** Auto vectorization may be a bit easier with separate loops, too.
*/
#define kd ((float)k)

  sum = 0.0f;
  for (k = 0; k <= n; k++) {  /* powf(2.0f/3.0f, kd) inlined */
    float x = 1.0f, q = 2.0f/3.0f;
    int j = k;
    for (;;) { if (j & 1) x *= q; if ((j >>= 1) == 0) break; q = q*q; }
    sum += x;
  }
  printf("%.9f\t(2/3)^k\n", sum);

  sum = 0.0f;
  for (k = 1 ; k <= n; k++) sum += 1/sqrtf(kd);  /* aka powf(kd, -0.5f) */
  printf("%.9f\tk^-0.5f\n", sum);

  sum = 0.0f;
  for (k = 1; k <= n; k++) sum += 1.0f/(kd*(kd+1.0f));
  printf("%.9f\t1/k(k+1)\n", sum);

  sum = 0.0f;
  for (k = 1; k <= n; k++) {
    float sk = sinf(kd);
    sum += 1.0f/(kd*kd*kd*sk*sk);
  }
  printf("%.9f\tFlint Hills\n", sum);

  sum = 0.0f;
  for (k = 1; k <= n; k++) {
    float ck = cosf(kd);
    sum += 1.0f/((kd*kd)*kd*ck*ck);
  }
  printf("%.9f\tCookson Hills\n", sum);

  sum = 0.0f;
  for (k = 1; k <= n; k++) sum += 1.0f/kd;
  printf("%.9f\tHarmonic\n", sum);

  sum = 0.0f;
  for (k = 1; k <= n; k++) sum += 1.0f/(kd*kd);
  printf("%.9f\tRiemann Zeta\n", sum);

  sum = 0.0f;
  for (k = 1; k <= n-1; k += 2) sum += 1.0f/kd;
  for (k = 2; k <= n; k += 2) sum -= 1.0f/kd;
  printf("%.9f\tAlternating Harmonic\n", sum);

  sum = 0.0f;
  for (k = 1; k <= 2*n-1; k += 4) sum += 1.0f/kd;
  for (k = 3; k <= 2*n; k += 4) sum -= 1.0f/kd;
  printf("%.9f\tGregory\n", sum);

  return 0;
}

