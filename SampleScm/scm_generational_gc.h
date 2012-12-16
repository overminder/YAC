#ifndef SCM_GENERATIONAL_GC_H
#define SCM_GENERATIONAL_GC_H

#include "scm_gc.h"

#define REM_SET_SIZE (1 * KB)

/* Space numbers */
#define SP_SMALL 1
#define SP_LARGE 2

#define SPACEOF(p) \
    ((GcHeader *) (p))->gcspaceno

typedef struct {
    ScmPtr next;
    ScmPtr limit;
    ScmPtr fr_sp;
    ScmPtr to_sp;
    ScmPtr copy_ptr;
    size_t size;
} Space;

typedef struct {
    Space small;
    Space large;
    ScmPtr root_stack[ROOT_STACK_SIZE];
    ScmPtr *root_stack_ptr;
    ScmPtr *global_roots[ROOT_STACK_SIZE];
    ScmPtr **global_root_ptr;
    ScmPtr remembered_set[REM_SET_SIZE];
    ScmPtr *rem_set_ptr;

    int n_allocs; size_t n_bytes_alloc;
    int n_minor_collects; size_t n_bytes_minor_collect;
    int n_major_collects; size_t n_bytes_major_collect;
    int n_remembers;
} Heap;

extern Heap *Scm_CurrHeap;

#endif  /* SCM_GENERATIONAL_GC_H */

