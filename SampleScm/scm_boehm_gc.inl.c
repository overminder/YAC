#ifndef SCM_BOEHM_GC_INL_C
#define SCM_BOEHM_GC_INL_C

#include <gc/gc.h>
#include "scm_boehm_gc.h"

inline void
Scm_PushGcRoot(ScmPtr wat) {
    *(Scm_CurrHeap->root_stack_ptr) = wat;
    Scm_CurrHeap->root_stack_ptr++;
}

inline ScmPtr
Scm_PopGcRoot(void) {
    ScmPtr res;
    Scm_CurrHeap->root_stack_ptr--;
    res = *(Scm_CurrHeap->root_stack_ptr);
    *(Scm_CurrHeap->root_stack_ptr) = 0;
    return res;
}

inline ScmPtr
Scm_GcAlloc(size_t size) {
    return (ScmPtr) GC_MALLOC(size);
}

#endif  /* SCM_BOEHM_GC_INL_C */

