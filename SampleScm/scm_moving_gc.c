#include "scm_runtime.h"
#include "scm_moving_gc.h"

Heap *Scm_CurrHeap;

#define curr_heap Scm_CurrHeap

static void _MarkPointer(GcHeader *);
static void _CopyPointer(GcHeader *);
static void _RedirectInteriorPointer(GcHeader *);

void
Scm_GcInit() {
    size_t size = 3 * MB;
    curr_heap = malloc(sizeof(Heap));
    curr_heap->space_size = size;
    curr_heap->from_space = (ScmPtr) malloc(size);
    curr_heap->next_alloc = curr_heap->from_space;
    curr_heap->limit = curr_heap->from_space + size;
    curr_heap->to_space = (ScmPtr) malloc(size);
    //curr_heap->to_space = 0;
    curr_heap->root_stack_ptr = curr_heap->root_stack;
    curr_heap->global_root_ptr = curr_heap->global_roots;

    curr_heap->n_allocs = curr_heap->n_bytes_alloc =
    curr_heap->n_collects = curr_heap->n_bytes_collect = 0;

    /* Push global variables since they could be overriden */
#define PUSH_GLOBALS(name) \
    Scm_AddGlobalGcRoot(&name);
SCM_PRELUDE_NAMES(PUSH_GLOBALS)
#undef PUSH_GLOBALS
}

void
Scm_GcFini() {
    free((void *) curr_heap->from_space);
    free((void *) curr_heap->to_space);
    free(curr_heap);
    curr_heap = 0;
}

void
Scm_GcSummary(void) {
    fprintf(stderr, "[Moving Gc Stat]\n");
    fprintf(stderr, "%d allocations, from which %ld bytes allocated.\n",
            curr_heap->n_allocs, curr_heap->n_bytes_alloc);
    fprintf(stderr, "%d collections, from which %ld bytes where collected.\n",
            curr_heap->n_collects, curr_heap->n_bytes_collect);
}

void
Scm_AddGlobalGcRoot(ScmPtr *g) {
    *(curr_heap->global_root_ptr) = g;
    curr_heap->global_root_ptr++;
}

void
Scm_GcCollect() {
    ScmPtr *iter;
    ScmPtr **iter2;
    ScmPtr v;
    GcHeader *h;

    /* Mark root set */
    for (iter = curr_heap->root_stack;
         iter < curr_heap->root_stack_ptr; ++iter) {
        v = *iter;
        if (Scm_IsPointer(v) && MARKOF(v) == GC_UNREACHABLE) {
            _MarkPointer((GcHeader *) v);
        }
    }

    for (iter2 = curr_heap->global_roots;
         iter2 < curr_heap->global_root_ptr; ++iter2) {
        v = **iter2;
        if (Scm_IsPointer(v) && MARKOF(v) == GC_UNREACHABLE) {
            _MarkPointer((GcHeader *) v);
        }
    }

    // XXX
    //curr_heap->to_space = (ScmPtr) malloc(curr_heap->space_size);

    /* Scan from-space and copy live objects to to-space */
    ScmPtr prev_obj_size;
    curr_heap->copy_ptr = curr_heap->to_space;
    for (v = curr_heap->from_space; v < curr_heap->next_alloc;
         v += prev_obj_size) {
        Scm_Assert(Scm_IsPointer(v));
        if (MARKOF(v) == GC_MARKED) {
            _CopyPointer((GcHeader *) v);
        }
        else {
            Scm_Assert(MARKOF(v) == GC_UNREACHABLE);
        }
        prev_obj_size = OBSIZE(v);
    }

    /* Redirect interior pointers in to-space */
    for (v = curr_heap->to_space; v < curr_heap->copy_ptr;
         v += prev_obj_size) {
        _RedirectInteriorPointer((GcHeader *) v);
        prev_obj_size = OBSIZE(v);
    }

    /* Mutate root stack */
    for (iter = curr_heap->root_stack;
         iter < curr_heap->root_stack_ptr; ++iter) {
        v = *iter;
        if (Scm_IsPointer(v) && MARKOF(v) == GC_MOVED_FROM) {
            *iter = COPIED_TO(v);
        }
    }

    for (iter2 = curr_heap->global_roots;
         iter2 < curr_heap->global_root_ptr; ++iter2) {
        v = **iter2;
        if (Scm_IsPointer(v) && MARKOF(v) == GC_MOVED_FROM) {
            **iter2 = COPIED_TO(v);
        }
    }
    size_t orig_usage = curr_heap->next_alloc - curr_heap->from_space;
    size_t curr_usage = curr_heap->copy_ptr - curr_heap->to_space;

    /* Swap two spaces */
    ScmPtr tmp = curr_heap->from_space;
    curr_heap->from_space = curr_heap->to_space;
    curr_heap->to_space = tmp;

    /* Stat */
#ifdef SCM_GC_STAT
    curr_heap->n_collects += 1;
    curr_heap->n_bytes_collect += orig_usage - curr_usage;
#endif

    // XXX
    //free((void *) curr_heap->to_space);
    //curr_heap->to_space = 0;

    curr_heap->next_alloc = curr_heap->copy_ptr;
    curr_heap->limit = curr_heap->from_space + curr_heap->space_size;
}

/* Recursive mark? */
static void
_MarkPointer(GcHeader *ptr) {
loop:
    ptr->gcmark = GC_MARKED;
    if (ptr->obtype == Scm_ClosureType) {
        int nb_upvals = (OBSIZE(ptr) - sizeof(ScmClosure)) >> 3;
        int i;
        ScmPtr *upvals = &(ScmClosure_GetUpvalAt(ptr, 0));
        for (i = 0; i < nb_upvals; ++i) {
            ScmPtr upval = upvals[i];
            if (Scm_IsPointer(upval) && MARKOF(upval) == GC_UNREACHABLE) {
                _MarkPointer((GcHeader *) upval);
            }
        }
    }
    else if (ptr->obtype == Scm_PairType) {
        ScmPtr iptr = ScmPair_Car(ptr);
        if (Scm_IsPointer(iptr) && MARKOF(iptr) == GC_UNREACHABLE) {
            _MarkPointer((GcHeader *) iptr);
        }
        iptr = ScmPair_Cdr(ptr);
        if (Scm_IsPointer(iptr) && MARKOF(iptr) == GC_UNREACHABLE) {
            ptr = (GcHeader *) iptr;
            goto loop;
        }
    }
    else {
        Scm_Fatal("Unknown type");
    }
}

static void
_CopyPointer(GcHeader *ptr) {
    size_t size = OBSIZE(ptr);
    ScmPtr copy_to = curr_heap->copy_ptr;
    Scm_Assert(Scm_IsPointer(copy_to));
    memcpy((void *) copy_to, ptr, size);
    curr_heap->copy_ptr = copy_to + size;

    ptr->gcmark = GC_MOVED_FROM;
    ptr->copied_to = copy_to;
    MARKOF(copy_to) = GC_MOVED_TO;
}

static void
_RedirectInteriorPointer(GcHeader *ptr) {
    Scm_Assert(ptr->gcmark == GC_MOVED_TO);
    ptr->gcmark = GC_UNREACHABLE;
    if (ptr->obtype == Scm_ClosureType) {
        int nb_upvals = (OBSIZE(ptr) - sizeof(ScmClosure)) >> 3;
        int i;
        ScmPtr *upvals = &(ScmClosure_GetUpvalAt(ptr, 0));
        for (i = 0; i < nb_upvals; ++i) {
            ScmPtr upval = upvals[i];
            if (Scm_IsPointer(upval)) {
               if (MARKOF(upval) == GC_MOVED_FROM) {
                   upvals[i] = COPIED_TO(upval);
               }
               else {
                   Scm_Assert(MARKOF(upval) == GC_UNREACHABLE ||
                              MARKOF(upval) == GC_NOTONHEAP);
               }
            }
        }
    }
    else if (ptr->obtype == Scm_PairType) {
        ScmPtr iptr = ScmPair_Car(ptr);
        if (Scm_IsPointer(iptr)) {
           if (MARKOF(iptr) == GC_MOVED_FROM) {
               ScmPair_Car(ptr) = COPIED_TO(iptr);
           }
           else {
               Scm_Assert(MARKOF(iptr) == GC_UNREACHABLE ||
                          MARKOF(iptr) == GC_NOTONHEAP);
           }
        }
        iptr = ScmPair_Cdr(ptr);
        if (Scm_IsPointer(iptr)) {
            if (MARKOF(iptr) == GC_MOVED_FROM) {
                ScmPair_Cdr(ptr) = COPIED_TO(iptr);
            }
           else {
               Scm_Assert(MARKOF(iptr) == GC_UNREACHABLE ||
                          MARKOF(iptr) == GC_NOTONHEAP);
           }
        }
    }
    else {
        Scm_Fatal("Unknown type");
    }
}


