#include "scm_runtime.h"

#define KB (1024)
#define MB (KB * KB)
#define ROOT_STACK_SIZE (1 * KB)

#define GC_NOTONHEAP 0
#define GC_UNREACHABLE 1
#define GC_MARKED 2
#define GC_MOVED_FROM 3
#define GC_MOVED_TO 4

typedef struct {
    SCM_HEADER;
    ScmPtr copied_to;
} GcHeader;

#define MARKOF(p) \
    ((GcHeader *) (p))->gcmark

#define OBSIZE(p) \
    ((GcHeader *) (p))->obsize

#define COPIED_TO(p) \
    ((GcHeader *) (p))->copied_to

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
} Heap;

static Heap *curr_heap;

void
Scm_GcInit() {
    size_t size = 256 * KB;
    curr_heap = malloc(sizeof(Heap));
    curr_heap->space_size = size;
    curr_heap->from_space = (ScmPtr) malloc(size);
    curr_heap->next_alloc = curr_heap->from_space;
    curr_heap->limit = curr_heap->from_space + size;
    curr_heap->to_space = (ScmPtr) malloc(size);
    curr_heap->root_stack_ptr = curr_heap->root_stack;
    curr_heap->global_root_ptr = curr_heap->global_roots;
}

void
Scm_GcFini() {
    free((void *) curr_heap->from_space);
    free((void *) curr_heap->to_space);
    free(curr_heap);
    curr_heap = 0;
}

void
Scm_AddGlobalGcRoot(ScmPtr *g) {
    *(curr_heap->global_root_ptr) = g;
    curr_heap->global_root_ptr++;
}

void
Scm_PushGcRoot(ScmPtr wat) {
    *(curr_heap->root_stack_ptr) = wat;
    curr_heap->root_stack_ptr++;
}

ScmPtr
Scm_PopGcRoot() {
    ScmPtr res;
    curr_heap->root_stack_ptr--;
    res = *(curr_heap->root_stack_ptr);
    *(curr_heap->root_stack_ptr) = 0;
    return res;
}

ScmPtr
Scm_GcAlloc(size_t size) {
    ScmPtr next_alloc = curr_heap->next_alloc;
    GcHeader *ptr = (GcHeader *)next_alloc;
    next_alloc += size;
    if (next_alloc >= curr_heap->limit) {
        return 0;
    }
    ptr->gcmark = GC_UNREACHABLE;
    ptr->obsize = size;
    curr_heap->next_alloc = next_alloc;
    return (ScmPtr) ptr;
}

static void _MarkPointer(GcHeader *);
static void _CopyPointer(GcHeader *);
static void _RedirectInteriorPointer(GcHeader *);

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

    /* Scan from-space and copy live objects to to-space */
    ScmPtr prev_obj_size;
    curr_heap->copy_ptr = curr_heap->to_space;
    for (v = curr_heap->from_space; v < curr_heap->next_alloc;
         v += prev_obj_size) {
        if (MARKOF(v) == GC_MARKED) {
            _CopyPointer((GcHeader *) v);
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

    /* Swap two spaces */
    ScmPtr tmp = curr_heap->from_space;
    curr_heap->from_space = curr_heap->to_space;
    curr_heap->to_space = tmp;

    curr_heap->next_alloc = curr_heap->copy_ptr;
    curr_heap->limit = curr_heap->from_space + curr_heap->space_size;
}

/* Recursive mark? */
static void
_MarkPointer(GcHeader *ptr) {
    ptr->gcmark = GC_MARKED;
    if (ptr->obtype == Scm_ClosureType) {
        int nb_upvals = (ptr->obsize - sizeof(ScmClosure)) >> 3;
        int i;
        ScmPtr *upvals = &(ScmClosure_UpvalAt(ptr, 0));
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
            _MarkPointer((GcHeader *) iptr);
        }
    }
    else {
        Scm_Fatal("Unknown type");
    }
}

static void
_CopyPointer(GcHeader *ptr) {
    size_t size = ptr->obsize;
    ScmPtr copy_to = curr_heap->copy_ptr;
    memcpy((void *) copy_to, ptr, size);
    curr_heap->copy_ptr = copy_to + size;

    ptr->gcmark = GC_MOVED_FROM;
    ptr->copied_to = copy_to;
    MARKOF(copy_to) = GC_MOVED_TO;
}

static void
_RedirectInteriorPointer(GcHeader *ptr) {
    if (ptr->gcmark != GC_MOVED_TO) {
        Scm_Fatal("unexpected gcmark");
    }
    ptr->gcmark = GC_UNREACHABLE;
    if (ptr->obtype == Scm_ClosureType) {
        int nb_upvals = (ptr->obsize - sizeof(ScmClosure)) >> 3;
        int i;
        ScmPtr *upvals = &(ScmClosure_UpvalAt(ptr, 0));
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


