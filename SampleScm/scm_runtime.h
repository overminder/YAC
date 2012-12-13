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
    int16_t gcmark;     \
    int32_t obsize

#define Scm_ClosureType 1

typedef struct {
    SCM_HEADER;
} ScmHeapObj;

typedef struct {
    SCM_HEADER;
    void *code;
    ScmPtr upvals[0];
} ScmClosure;

typedef void (*ScmFuncPtr1) (ScmPtr, ScmPtr);
typedef void (*ScmFuncPtr2) (ScmPtr, ScmPtr, ScmPtr);
typedef void (*ScmFuncPtr3) (ScmPtr, ScmPtr, ScmPtr, ScmPtr);
typedef void (*ScmFuncPtr4) (ScmPtr, ScmPtr, ScmPtr, ScmPtr, ScmPtr);

#define Scm_HeapObjType(o) \
    ((ScmHeapObj *) (o))->obtype

#define Scm_TailCall1(clo, arg1) \
    ((ScmFuncPtr1) ScmClosure_Code(clo))(clo, arg1)

#define Scm_TailCall2(clo, arg1, arg2) \
    ((ScmFuncPtr2) ScmClosure_Code(clo))(clo, arg1, arg2)

#define Scm_TailCall3(clo, arg1, arg2, arg3) \
    ((ScmFuncPtr3) ScmClosure_Code(clo))(clo, arg1, arg2, arg3)

#define Scm_TailCall4(clo, arg1, arg2, arg3, arg4) \
    ((ScmFuncPtr4) ScmClosure_Code(clo))(clo, arg1, arg2, arg3, arg4)

#define ScmClosure_UpvalAt(clo, n) \
    (((ScmClosure *) (clo))->upvals[n])

#define ScmClosure_Code(clo) \
    (((ScmClosure *) (clo))->code)

// XXXX XX01
#define Scm_MkInt(i) \
    (((i) << 2) + 1)

#define Scm_GetInt(i) \
    ((i) >> 2)

#define Scm_IsPointer(p) \
    ((((ScmPtr) (p)) & 0x3) == 0)

// Should be kept in sync with scm_gc (gcmark=0: not-on-heap)
#define Scm_MkSuperComb(funcptr) \
    { Scm_ClosureType, 0, 0, &funcptr }

#define Scm_Fatal(wat) \
    fprintf(stderr, "*FATAL ERROR*(%s:%d:%s): %s\n",    \
            __FILE__, __LINE__, __func__, wat);         \
    exit(1)

void Scm_GcInit();
void Scm_GcFini();
void Scm_PushGcRoot(ScmPtr);
ScmPtr Scm_PopGcRoot();
ScmPtr Scm_GcAlloc(size_t);
void Scm_GcCollect();

extern ScmPtr display;
extern ScmPtr zg;  /* + */
extern ScmPtr zh;  /* - */
extern ScmPtr zl;  /* < */
extern ScmPtr halt;

// XXXX XX10
#define Scm_True 2
#define Scm_False 6
#define Scm_Unspecified 10
#define Scm_Unbound 14

extern ScmPtr Mainzkmain;  /* exported by app code */

#endif  /* SCM_RUNTIME_H */

