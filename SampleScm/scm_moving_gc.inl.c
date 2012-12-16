#include "scm_moving_gc.h"

inline void
Scm_PushGcRoot(ScmPtr wat) {
    *(Scm_CurrHeap->root_stack_ptr) = wat;
    Scm_CurrHeap->root_stack_ptr++;
}

inline ScmPtr
Scm_PopGcRoot() {
    ScmPtr res;
    Scm_CurrHeap->root_stack_ptr--;
    res = *(Scm_CurrHeap->root_stack_ptr);
    return res;
}

inline ScmPtr
Scm_GcAlloc(size_t size) {
    ScmPtr next_alloc = Scm_CurrHeap->next_alloc;
    Scm_Assert(Scm_IsPointer(next_alloc));
    GcHeader *ptr = (GcHeader *) next_alloc;
    next_alloc += size;
    if (next_alloc >= Scm_CurrHeap->limit) {
        return 0;
    }

#ifdef SCM_GC_STAT
    Scm_CurrHeap->n_allocs += 1;
    Scm_CurrHeap->n_bytes_alloc += size;
#endif

    ptr->gcmark = GC_UNREACHABLE;
    ptr->obsize = size;
    Scm_CurrHeap->next_alloc = next_alloc;
    return (ScmPtr) ptr;
}

