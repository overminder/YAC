#include "scm_runtime.h"
#include "scm_boehm_gc.h"

#define curr_heap Scm_CurrHeap
Heap *Scm_CurrHeap;

void Scm_GcInit(void) {
    curr_heap = malloc(sizeof(Heap));
    curr_heap->root_stack_ptr = curr_heap->root_stack;
    curr_heap->global_root_ptr = curr_heap->global_roots;
}

void Scm_GcFini(void) {
    free(curr_heap);
}

void Scm_AddGlobalGcRoot(ScmPtr *x) {
}

void Scm_GcCollect(void) {
}

void Scm_GcSummary(void) {
}

