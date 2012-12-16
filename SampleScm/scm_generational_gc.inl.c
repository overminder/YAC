#ifndef SCM_GENERATIONAL_GC_INL_C
#define SCM_GENERATIONAL_GC_INL_C

#include "scm_generational_gc.h"

inline void
Scm_GcWriteBarrier(ScmPtr container, ScmPtr new_item) {
    if (SPACEOF(container) == SP_LARGE &&
        Scm_IsPointer(new_item) && SPACEOF(new_item) == SP_SMALL) {
        *Scm_CurrHeap->rem_set_ptr = container;
        ++Scm_CurrHeap->rem_set_ptr;
#ifdef SCM_GC_STAT
    Scm_CurrHeap->n_remembers += 1;
#endif
    }
}

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
    ScmPtr next = Scm_CurrHeap->small.next;
    GcHeader *ptr = (GcHeader *) next;
    next += size;
    if (next >= Scm_CurrHeap->small.limit) {
        return 0;
    }
#ifdef SCM_GC_STAT
    Scm_CurrHeap->n_allocs += 1;
    Scm_CurrHeap->n_bytes_alloc += size;
#endif
    ptr->gcmark = GC_UNREACHABLE;
    ptr->gcspaceno = SP_SMALL;
    ptr->obsize = size;
    Scm_CurrHeap->small.next = next;
    return (ScmPtr) ptr;
}

#endif  /* SCM_GENERATIONAL_GC_INL_C */

