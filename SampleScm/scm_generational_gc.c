#include "scm_runtime.h"
#include "scm_generational_gc.h"

#define curr_heap Scm_CurrHeap
Heap *Scm_CurrHeap;

static void _AllocSpace(Space *self, size_t size, int need_to_sp);
static void _FreeSpace(Space *self);
static void _MajorCollect(void);
static void _MarkInterior(GcHeader *, int is_minor);
static void _CopyAndIncrDest(GcHeader *src, ScmPtr *dest);
static void _RedirectInteriorPointer(GcHeader *);
static void _MarkRootSet(int is_minor);
static void _MutateRootSet(void);

#define MINOR 1
#define MAJOR 0

#define SHALL_MARK(p) \
    (Scm_IsPointer(p) && MARKOF(p) == GC_UNREACHABLE && \
     (!is_minor || (is_minor && SPACEOF(p) == SP_SMALL)))

void
Scm_GcInit(void) {
    curr_heap = malloc(sizeof(Heap));
    _AllocSpace(&curr_heap->small, 128 * KB, 0);  /* half of L2 cache */
    _AllocSpace(&curr_heap->large, 3 * MB, 1);   /* 66% of L3 cache */
    curr_heap->root_stack_ptr = curr_heap->root_stack;
    curr_heap->global_root_ptr = curr_heap->global_roots;
    curr_heap->rem_set_ptr = curr_heap->remembered_set;

    curr_heap->n_allocs = curr_heap->n_bytes_alloc =
    curr_heap->n_minor_collects = curr_heap->n_bytes_minor_collect = 
    curr_heap->n_major_collects = curr_heap->n_bytes_major_collect = 
    curr_heap->n_remembers = 0;
}

void
Scm_GcFini(void) {
    _FreeSpace(&curr_heap->small);
    _FreeSpace(&curr_heap->large);
    free(curr_heap);
    curr_heap = 0;
}

void
Scm_GcSummary(void) {
    fprintf(stderr, "[Generational Gc Stat]\n");
    fprintf(stderr, "%d pointers remembered.\n", curr_heap->n_remembers);
    fprintf(stderr, "%d allocations, from which %ld bytes allocated.\n",
            curr_heap->n_allocs, curr_heap->n_bytes_alloc);
    fprintf(stderr, "%d minor collections, "
            "from which %ld bytes where collected.\n",
            curr_heap->n_minor_collects, curr_heap->n_bytes_minor_collect);
    fprintf(stderr, "%d MAJOR collections, "
            "from which %ld bytes where collected.\n",
            curr_heap->n_major_collects, curr_heap->n_bytes_major_collect);
}

void
Scm_AddGlobalGcRoot(ScmPtr *g) {
    *(curr_heap->global_root_ptr) = g;
    curr_heap->global_root_ptr++;
}

void
Scm_GcCollect(void) {
    ScmPtr *iter;
    ScmPtr **iter2;
    ScmPtr v;
    GcHeader *h;

    /* Check if is major collect */
    if (curr_heap->large.limit - curr_heap->large.next <
        curr_heap->small.size) {
        _MajorCollect();
        return;
    }

    _MarkRootSet(MINOR);

    /* Scan and copy live objects in small.fr_sp to large.fr_sp.
       Just like ordinary alloc */
    ScmPtr prev_obj_size;
    ScmPtr newly_copied_begin = curr_heap->large.next;  /* save for later use */
    for (v = curr_heap->small.fr_sp; v < curr_heap->small.next;
         v += prev_obj_size) {
        if (MARKOF(v) == GC_MARKED) {
            _CopyAndIncrDest((GcHeader *) v, &(curr_heap->large.next));
        }
        prev_obj_size = OBSIZE(v);
    }

    /* Redirect interior pointers, reset gcmark to UNREACHABLE,
       and set spaceno to SP_LARGE for all newly copied objects. */
    for (v = newly_copied_begin; v < curr_heap->large.next;
         v += prev_obj_size) {
        Scm_Assert(MARKOF(v) == GC_MOVED_TO);
        MARKOF(v) = GC_UNREACHABLE;
        SPACEOF(v) = SP_LARGE;
        _RedirectInteriorPointer((GcHeader *) v);
        prev_obj_size = OBSIZE(v);
    }

    /* Redirect interior pointers for objects in the remembered set */
    for (iter = curr_heap->remembered_set;
         iter < curr_heap->rem_set_ptr; ++iter) {
        _RedirectInteriorPointer((GcHeader *) *iter);
    }
    /* Clear remembered set, since survived objects are already
       in the elder generation space. */
    curr_heap->rem_set_ptr = curr_heap->remembered_set;

    _MutateRootSet();

    size_t orig_usage = curr_heap->small.next - curr_heap->small.fr_sp;
    size_t curr_usage = newly_copied_begin - curr_heap->large.next;

#ifdef SCM_GC_STAT
    curr_heap->n_minor_collects += 1;
    curr_heap->n_bytes_minor_collect += orig_usage - curr_usage;
#endif

    /* Clear small */
    curr_heap->small.next = curr_heap->small.fr_sp;
}

static void
_AllocSpace(Space *self, size_t size, int need_to_sp) {
    self->size = size;
    self->fr_sp = (ScmPtr) malloc(size);
    self->to_sp = need_to_sp ? (ScmPtr) malloc(size) : 0;
    self->next = self->fr_sp;
    self->limit = self->fr_sp + size;
}

static void
_FreeSpace(Space *self) {
    free((void *) self->fr_sp);
    free((void *) self->to_sp);
}

static void
_MajorCollect(void) {
    ScmPtr *iter;
    ScmPtr **iter2;
    ScmPtr v;
    GcHeader *h;

    _MarkRootSet(MAJOR);

    /* Scan and copy live objects in large.fr_sp to large.to_sp. */
    ScmPtr prev_obj_size;
    curr_heap->large.copy_ptr = curr_heap->large.to_sp;  /* init copy ptr */
    for (v = curr_heap->large.fr_sp; v < curr_heap->large.next;
         v += prev_obj_size) {
        if (MARKOF(v) == GC_MARKED) {
            _CopyAndIncrDest((GcHeader *) v, &(curr_heap->large.copy_ptr));
        }
        prev_obj_size = OBSIZE(v);
    }

    /* Scan and copy live objects in small.fr_sp to large.to_sp,
       also change their generation no. */
    for (v = curr_heap->small.fr_sp; v < curr_heap->small.next;
         v += prev_obj_size) {
        if (MARKOF(v) == GC_MARKED) {
            Scm_Assert(curr_heap->large.copy_ptr + OBSIZE(v) <
                       curr_heap->large.to_sp + curr_heap->large.size);
            _CopyAndIncrDest((GcHeader *) v, &(curr_heap->large.copy_ptr));
            SPACEOF(COPIED_TO(v)) = SP_LARGE;
        }
        prev_obj_size = OBSIZE(v);
    }

    /* Redirect interior pointers and reset gcmark to UNREACHABLE */
    for (v = curr_heap->large.to_sp; v < curr_heap->large.copy_ptr;
         v += prev_obj_size) {
        Scm_Assert(MARKOF(v) == GC_MOVED_TO);
        MARKOF(v) = GC_UNREACHABLE;
        _RedirectInteriorPointer((GcHeader *) v);
        prev_obj_size = OBSIZE(v);
    }

    _MutateRootSet();

    size_t orig_usage = curr_heap->small.next - curr_heap->small.fr_sp +
                        curr_heap->large.next - curr_heap->large.fr_sp;
    size_t curr_usage = curr_heap->large.copy_ptr - curr_heap->large.to_sp;

#ifdef SCM_GC_STAT
    curr_heap->n_major_collects += 1;
    curr_heap->n_bytes_major_collect += orig_usage - curr_usage;
#endif

    /* Clear small */
    curr_heap->small.next = curr_heap->small.fr_sp;

    /* Swap large */
    ScmPtr tmp = curr_heap->large.fr_sp;
    curr_heap->large.fr_sp = curr_heap->large.to_sp;
    curr_heap->large.to_sp = tmp;
    curr_heap->large.next = curr_heap->large.copy_ptr;
    curr_heap->large.limit = curr_heap->large.fr_sp + curr_heap->large.size;

    //fprintf(stderr, "#%d major collection finished, curr usage=(%ld/%ld)\n",
    //        curr_heap->n_major_collects, curr_usage, curr_heap->large.size);
}

/* Recursive mark? */
static void
_MarkInterior(GcHeader *ptr, int is_minor) {
loop:
    if (ptr->obtype == Scm_ClosureType) {
        int nb_upvals = (ptr->obsize - sizeof(ScmClosure)) >> 3;
        int i;
        ScmPtr *upvals = &ScmClosure_GetUpvalAt(ptr, 0);
        for (i = 0; i < nb_upvals; ++i) {
            ScmPtr upval = upvals[i];
            if (SHALL_MARK(upval)) {
                MARKOF(upval) = GC_MARKED;
                _MarkInterior((GcHeader *) upval, is_minor);
            }
        }
    }
    else if (ptr->obtype == Scm_PairType) {
        ScmPtr iptr = ScmPair_Car(ptr);
        if (SHALL_MARK(iptr)) {
            MARKOF(iptr) = GC_MARKED;
            _MarkInterior((GcHeader *) iptr, is_minor);
        }
        iptr = ScmPair_Cdr(ptr);
        if (SHALL_MARK(iptr)) {
            MARKOF(iptr) = GC_MARKED;
            ptr = (GcHeader *) iptr;
            goto loop;
        }
    }
    else {
        Scm_Fatal("Unknown type");
    }
}

static void
_CopyAndIncrDest(GcHeader *src, ScmPtr *dest) {
    size_t size = src->obsize;
    memcpy((void *) *dest, src, size);

    src->gcmark = GC_MOVED_FROM;
    src->copied_to = *dest;
    MARKOF(*dest) = GC_MOVED_TO;

    *dest += size;
}

static void
_RedirectInteriorPointer(GcHeader *ptr) {
    if (ptr->obtype == Scm_ClosureType) {
        int nb_upvals = (ptr->obsize - sizeof(ScmClosure)) >> 3;
        int i;
        ScmPtr *upvals = &(ScmClosure_GetUpvalAt(ptr, 0));
        for (i = 0; i < nb_upvals; ++i) {
            ScmPtr upval = upvals[i];
            if (Scm_IsPointer(upval) && MARKOF(upval) == GC_MOVED_FROM) {
                upvals[i] = COPIED_TO(upval);
            }
        }
    }
    else if (ptr->obtype == Scm_PairType) {
        ScmPtr iptr = ScmPair_Car(ptr);
        if (Scm_IsPointer(iptr) && MARKOF(iptr) == GC_MOVED_FROM) {
            ScmPair_Car(ptr) = COPIED_TO(iptr);
        }
        iptr = ScmPair_Cdr(ptr);
        if (Scm_IsPointer(iptr) && MARKOF(iptr) == GC_MOVED_FROM) {
            ScmPair_Cdr(ptr) = COPIED_TO(iptr);
        }
    }
    else {
        Scm_Fatal("Unknown type");
    }
}

static void
_MarkRootSet(int is_minor) {
    ScmPtr *iter;
    ScmPtr **iter2;
    ScmPtr v;

    /* Mark local root set */
    for (iter = curr_heap->root_stack;
         iter < curr_heap->root_stack_ptr; ++iter) {
        v = *iter;
        if (SHALL_MARK(v)) {
            MARKOF(v) = GC_MARKED;
            _MarkInterior((GcHeader *) v, is_minor);
        }
    }

    /* Mark global root set */
    for (iter2 = curr_heap->global_roots;
         iter2 < curr_heap->global_root_ptr; ++iter2) {
        v = **iter2;
        if (SHALL_MARK(v)) {
            MARKOF(v) = GC_MARKED;
            _MarkInterior((GcHeader *) v, is_minor);
        }
    }

    if (is_minor) {
        /* Mark remembered set. Since they are old pointers, there
           is no need to test for IsPointer and SPACEOF */
        for (iter = curr_heap->remembered_set;
             iter < curr_heap->rem_set_ptr; ++iter) {
            _MarkInterior((GcHeader *) *iter, MINOR);
        }
    }
    else {
        /* Clear remembered set, since both spaces will be marked
           and collected this time... */
        curr_heap->rem_set_ptr = curr_heap->remembered_set;
    }
}

static void
_MutateRootSet(void) {
    ScmPtr *iter;
    ScmPtr **iter2;
    ScmPtr v;

    /* Mutate root stack */
    for (iter = curr_heap->root_stack;
         iter < curr_heap->root_stack_ptr; ++iter) {
        v = *iter;
        if (Scm_IsPointer(v) && MARKOF(v) == GC_MOVED_FROM) {
            *iter = COPIED_TO(v);
        }
    }

    /* Mutate global set */
    for (iter2 = curr_heap->global_roots;
         iter2 < curr_heap->global_root_ptr; ++iter2) {
        v = **iter2;
        if (Scm_IsPointer(v) && MARKOF(v) == GC_MOVED_FROM) {
            **iter2 = COPIED_TO(v);
        }
    }
}

