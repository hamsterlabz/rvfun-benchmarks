extern void *bmperf_alloc(void);
extern void bmperf_snap(void *);
extern void bmperf_report(void *, void *);
/* binarytrees, C: the Benchmarks Game binary-trees kernel (Kevin Carson's
   shootout entry, bench/binarytrees/binarytrees.gcc), UNCHANGED in structure.
   Adapted for bare metal exactly as the other cbench programs are: N is fixed
   instead of read from argv, pow(2,k) is the shift it always was (no libm),
   and the three printf lines are folded into one checksum so every version can be
   compared by a single number.  N = 8 keeps the run inside the sim budget. */
#include <stdio.h>
#include <stdlib.h>

#define N 8
typedef struct tn { struct tn *left, *right; long item; } treeNode;

static treeNode *NewTreeNode(treeNode *left, treeNode *right, long item) {
    treeNode *n = (treeNode *)malloc(sizeof(treeNode));
    n->left = left; n->right = right; n->item = item;
    return n;
}
static long ItemCheck(treeNode *tree) {
    if (tree->left == NULL) return tree->item;
    return tree->item + ItemCheck(tree->left) - ItemCheck(tree->right);
}
static treeNode *BottomUpTree(long item, unsigned depth) {
    if (depth > 0)
        return NewTreeNode(BottomUpTree(2*item-1, depth-1),
                           BottomUpTree(2*item,   depth-1), item);
    return NewTreeNode(NULL, NULL, item);
}
static void DeleteTree(treeNode *tree) {
    if (tree->left != NULL) { DeleteTree(tree->left); DeleteTree(tree->right); }
    free(tree);
}
static int bench(void) {
    unsigned depth, minDepth = 4, maxDepth, stretchDepth;
    treeNode *stretchTree, *longLivedTree, *tempTree;
    long sum = 0;
    maxDepth = ((minDepth + 2) > N) ? minDepth + 2 : N;
    stretchDepth = maxDepth + 1;
    stretchTree = BottomUpTree(0, stretchDepth);
    sum += ItemCheck(stretchTree);
    DeleteTree(stretchTree);
    longLivedTree = BottomUpTree(0, maxDepth);
    for (depth = minDepth; depth <= maxDepth; depth += 2) {
        long i, iterations = 1L << (maxDepth - depth + minDepth), check = 0;
        for (i = 1; i <= iterations; i++) {
            tempTree = BottomUpTree(i, depth);
            check += ItemCheck(tempTree); DeleteTree(tempTree);
            tempTree = BottomUpTree(-i, depth);
            check += ItemCheck(tempTree); DeleteTree(tempTree);
        }
        sum += check;
    }
    sum += ItemCheck(longLivedTree);
    printf("%ld\n", sum);
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
