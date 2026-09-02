extern void *bmperf_alloc(void);
extern void bmperf_snap(void *);
extern void bmperf_report(void *, void *);
/* fannkuchredux, C: n = 6, prints checksum then max flips (49 then 10). */
#include <stdio.h>
#define N 6
static int perm[N], perm1[N], count[N];
static void rev(int i, int j) { while (i < j) { int t = perm[i]; perm[i] = perm[j]; perm[j] = t; i++; j--; } }
static int flips(void) { int f = 0; while (perm[0] != 0) { rev(0, perm[0]); f++; } return f; }
static int nextperm(int r) {
    while (r < N) {
        int p0 = perm1[0], i;
        for (i = 0; i < r; i++) perm1[i] = perm1[i+1];
        perm1[r] = p0;
        count[r]--;
        if (count[r] > 0) return 1;
        count[r] = r + 1;
        r++;
    }
    return 0;
}
static int bench(void) {
    int i, even = 0, maxflips = 0, checksum = 0;
    for (i = 0; i < N; i++) { perm1[i] = i; count[i] = i + 1; }
    for (;;) {
        int f;
        for (i = 0; i < N; i++) perm[i] = perm1[i];
        f = flips();
        if (f > maxflips) maxflips = f;
        checksum = even == 0 ? checksum + f : checksum - f;
        if (nextperm(1) != 1) break;
        even = 1 - even;
    }
    printf("%d%d\n", checksum, maxflips);
    return 0;
}

int main(void) {
    void *s = bmperf_alloc(), *e = bmperf_alloc();
    bmperf_snap(s);
    int r = bench();
    bmperf_snap(e);
    bmperf_report(s, e);
    return r;
}
