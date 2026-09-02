/* PORT (Double->Float rule, all versions binary32 on the rv32f FPU):
 * double -> float, double literals -> f-suffixed, libm calls ->
 * f-variants.  No other change from the upstream source. */
/* The Great Computer Language Shootout
   http://shootout.alioth.debian.org/

   contributed by Greg Buchholz 
   Optimized by Paul Hsieh
   compile:  gcc -O2 -o harmonic harmonic.c
*/
#include<stdio.h>
#include<stdlib.h>

int main (int argc, char **argv)
{
    float i=1, sum=0;
    int n;

    for(n = atoi(argv[1]); n > 0; n--, i++)
        sum += 1/i;

    printf("%.9f\n", sum);
    return 0;
}

