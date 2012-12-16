#ifndef SCM_RUNTIME_H
#define SCM_RUNTIME_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stddef.h>
#include <inttypes.h>

typedef intptr_t ScmPtr;

#define SCM_HEADER      \
    int16_t obtype;     \
    int8_t gcmark;      \
    int8_t gcspaceno;   \
    int32_t obsize

#define Scm_ClosureType 1
#define Scm_PairType 2

typedef struct {
    SCM_HEADER;
} ScmHeapObj;

typedef struct {
    SCM_HEADER;
    void *code;
    const char *name;
    ScmPtr arity;
    ScmPtr upvals[0];
} ScmClosure;

// Should be kept in sync with scm_gc
#define Scm_MkSuperComb(funcptr, name, arity) { \
    Scm_ClosureType, \
    0, /* gcmark = 0: not on the heap */ \
    0, /* gcspaceno */ \
    0, /* obsize */ \
    &funcptr, \
    name, \
    arity \
}

typedef struct {
    SCM_HEADER;
    ScmPtr p_car;
    ScmPtr p_cdr;
} ScmPair;

/* Mostly for debug */
void Scm_RawDisplay(ScmPtr a);

typedef void (*ScmFuncPtr1) (ScmPtr, ScmPtr);
typedef void (*ScmFuncPtr2) (ScmPtr, ScmPtr, ScmPtr);
typedef void (*ScmFuncPtr3) (ScmPtr, ScmPtr, ScmPtr, ScmPtr);
typedef void (*ScmFuncPtr4) (ScmPtr, ScmPtr, ScmPtr, ScmPtr, ScmPtr);

#define Scm_SetLocal(v, e) \
    v = e

#define Scm_SetGlobal(v, e) \
    v = e

#define Scm_TailCall1(clo, arg1) \
    Scm_Assert(Scm_IsPointer(clo) && \
               Scm_HeapObjType(clo) == Scm_ClosureType); \
    Scm_Assert(ScmClosure_Arity(clo) == 1); \
    ((ScmFuncPtr1) ScmClosure_Code(clo))(clo, arg1)

#define Scm_TailCall2(clo, arg1, arg2) \
    Scm_Assert(Scm_IsPointer(clo) && \
               Scm_HeapObjType(clo) == Scm_ClosureType); \
    Scm_Assert(ScmClosure_Arity(clo) == 2); \
    ((ScmFuncPtr2) ScmClosure_Code(clo))(clo, arg1, arg2)

#define Scm_TailCall3(clo, arg1, arg2, arg3) \
    Scm_Assert(Scm_IsPointer(clo) && \
               Scm_HeapObjType(clo) == Scm_ClosureType); \
    Scm_Assert(ScmClosure_Arity(clo) == 3); \
    ((ScmFuncPtr3) ScmClosure_Code(clo))(clo, arg1, arg2, arg3)

#define Scm_TailCall4(clo, arg1, arg2, arg3, arg4) \
    Scm_Assert(Scm_IsPointer(clo) && \
               Scm_HeapObjType(clo) == Scm_ClosureType); \
    Scm_Assert(ScmClosure_Arity(clo) == 4); \
    ((ScmFuncPtr4) ScmClosure_Code(clo))(clo, arg1, arg2, arg3, arg4)

#define ScmClosure_GetUpvalAt(clo, n) \
    (((ScmClosure *) (clo))->upvals[n])

#define ScmClosure_Name(clo) \
    (((ScmClosure *) (clo))->name)

#define ScmClosure_Arity(clo) \
    (((ScmClosure *) (clo))->arity)

# define ScmClosure_SetUpvalAt(clo, n, v) \
    Scm_GcWriteBarrier(clo, v); \
    ScmClosure_GetUpvalAt(clo, n) = v


#define ScmClosure_Code(clo) \
    (((ScmClosure *) (clo))->code)

#define ScmPair_Car(p) \
    (((ScmPair *) (p))->p_car)

#define ScmPair_Cdr(p) \
    (((ScmPair *) (p))->p_cdr)

// XXXX XX01
#define Scm_IsInt(i) \
    ((i) & 0x3) == 1

#define Scm_MkInt(i) \
    (((i) << 2) + 1)

#define Scm_GetInt(i) \
    ((i) >> 2)

#define Scm_IsPointer(p) \
    ((((ScmPtr) (p)) & 0x3) == 0)

#define Scm_HeapObjType(o) \
    ((ScmHeapObj *) (o))->obtype

#define Scm_Fatal(wat) \
    fprintf(stderr, "**FATAL ERROR** (%s:%d:%s): %s\n",    \
            __FILE__, __LINE__, __func__, wat);         \
    exit(1)

#define Scm_Assert(expr) \
    do { \
        if (!(expr)) { \
            Scm_Fatal("Assertion failed (" #expr ")"); \
        } \
    } while (0)

#define SCM_PRELUDE_NAMES(V) \
    V(display) \
    V(newline) \
    V(zg)  /* + */ \
    V(zh)  /* - */ \
    V(zl)  /* < */ \
    V(halt) \
    V(cons) \
    V(car) \
    V(cdr) \
    V(setzhcarza)  /* set-car! */ \
    V(setzhcdrza)  /* set-cdr! */ \
    V(callzjcc)  /* call/cc */ \
    V(eqzo)  /* eq? */

#define EXPORT_NAME(name) \
    extern ScmPtr name;
SCM_PRELUDE_NAMES(EXPORT_NAME)
#undef EXPORT_NAME

// XXXX XX10
#define Scm_True 2
#define Scm_False 6
#define Scm_Unspecified 10
#define Scm_Unbound 14
#define Scm_Nil 18

extern ScmPtr Mainzkmain;  /* exported by app code */

/* GC settings */
#ifdef SCM_GC_FLAVOR_MOVING
/* Simple Moving GC */
# define Scm_GcWriteBarrier(clo, v) (void) clo; (void) v
#include "scm_moving_gc.inl.c"

#elif SCM_GC_FLAVOR_GENERATIONAL
/* Two-generation Moving GC */
#include "scm_generational_gc.inl.c"

#elif SCM_GC_FLAVOR_BOEHM
/* Conservative Boehm GC */
# define Scm_GcWriteBarrier(clo, v) (void) clo; (void) v
#include "scm_boehm_gc.inl.c"

#else
# error "Unknown gc flavor"
#endif  /* SCM_GC_FLAVOR */


#endif  /* SCM_RUNTIME_H */

