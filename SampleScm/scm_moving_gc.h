#ifndef SCM_MOVING_GC_H
#define SCM_MOVING_GC_H

#include "scm_gc.h"

typedef struct {
    ScmPtr next_alloc;
    ScmPtr limit;
    ScmPtr from_space;
    ScmPtr to_space;
    ScmPtr copy_ptr;
    size_t space_size;
    ScmPtr root_stack[ROOT_STACK_SIZE];
    ScmPtr *root_stack_ptr;
    ScmPtr *global_roots[ROOT_STACK_SIZE];
    ScmPtr **global_root_ptr;
    int n_allocs;
    int n_collects;
    size_t n_bytes_alloc;
    size_t n_bytes_collect;
} Heap;

extern Heap *Scm_CurrHeap;

#endif  /* SCM_MOVING_GC_H */

