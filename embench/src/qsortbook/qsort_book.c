/* qsort_book.c -- quicksort in the imperative shape, after the array version
 * developed in Hartel & Muller, "Functional C" (1999), section 7.4.2.
 *
 * The book's point in that section is the contrast: the same algorithm has a
 * list shape (7.4.1) and an array shape (7.4.2), and the array shape trades
 * copying for in-place mutation and index bookkeeping. This file is the array
 * shape; Qsort.hs is the list shape. Both sort the same data and return the
 * same order-sensitive checksum, so the only difference measured is the shape.
 *
 * Algorithm as the book develops it: the pivot is the leftmost element, and
 * the partition is THREE-WAY -- elements below the pivot end up at l..j,
 * elements above at i..r, and the run between j+1 and i-1 is exactly equal to
 * the pivot and needs no further sorting. Only the outer two ranges recurse.
 *
 * The code is mine, written from that description.
 */

#include "support.h"

#define LOCAL_SCALE_FACTOR 1
#define N 2000

static char data[N];

/* the same generator the Haskell side uses, so both sort identical input */
static void gen (char *a, int n)
{
  unsigned int s = 12345u;
  int i;
  for (i = 0; i < n; i++)
    {
      s = s * 1103515245u + 12345u;
      a[i] = (char) ((s >> 16) & 127);
    }
}

static void swap (char *a, int i, int j)
{
  char ai = a[i], aj = a[j];
  a[i] = aj;
  a[j] = ai;
}

/* three-way partition around the pivot value pv, over a[l..r].
   On return everything <= pv is at l..*j and everything >= pv is at *i..r. */
static void partition (char pv, char *a, int l, int r, int *ip, int *jp)
{
  int i = l, j = r;
  while (i <= j)
    {
      while (i <= r && a[i] < pv) i++;
      while (j >= l && a[j] > pv) j--;
      if (i <= j)
        {
          swap (a, i, j);
          i++;
          j--;
        }
    }
  *ip = i;
  *jp = j;
}

static void qsort_book (char *a, int l, int r)
{
  if (l >= r) return;
  {
    char pv = a[l];
    int i, j;
    partition (pv, a, l, r, &i, &j);
    qsort_book (a, l, j);
    qsort_book (a, i, r);
  }
}

/* order-sensitive: a checksum that ignores order would pass on unsorted data */
static int checksum (const char *a, int n)
{
  int h = 0, i;
  for (i = 0; i < n; i++)
    h = (int) ((unsigned int) h * 31u + (unsigned int) (unsigned char) a[i]);
  return h;
}

int sorted_ok;

static int benchmark_body (unsigned int lsf, unsigned int gsf)
{
  int h = 0;
  unsigned int c1, c2;

  for (c1 = 0; c1 < lsf; c1++)
    for (c2 = 0; c2 < gsf; c2++)
      {
        int i;
        gen (data, N);
        qsort_book (data, 0, N - 1);
        sorted_ok = 1;
        for (i = 1; i < N; i++)
          if (data[i - 1] > data[i]) sorted_ok = 0;
        h = checksum (data, N);
      }

  return h;
}

void warm_caches (int heat) { (void) benchmark_body (1, heat); }

int benchmark (void) { return benchmark_body (LOCAL_SCALE_FACTOR, GLOBAL_SCALE_FACTOR); }

void initialise_benchmark (void) { }

/* both versions must agree on the order-sensitive checksum, not merely sort */
int verify_benchmark (int r) { return sorted_ok && r == 66071136; }
