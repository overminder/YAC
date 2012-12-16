#ifndef SCM_BOEHM_GC_H
#define SCM_BOEHM_GC_H

#include "scm_gc.h"

typedef struct {
    ScmPtr root_stack[ROOT_STACK_SIZE];
    ScmPtr *root_stack_ptr;
    ScmPtr *global_roots[ROOT_STACK_SIZE];
    ScmPtr **global_root_ptr;
} Heap;

extern Heap *Scm_CurrHeap;

#endif  /* SCM_BOEHM_GC_H */

