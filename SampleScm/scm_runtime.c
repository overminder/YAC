#include "scm_runtime.h"

void
Scm_RawDisplay(ScmPtr a) {
    if (Scm_IsInt(a)) {
        printf("%ld", Scm_GetInt(a));
    }
    else if (a == Scm_True) {
        printf("#t");
    }
    else if (a == Scm_False) {
        printf("#f");
    }
    else if (a == Scm_Unbound) {
        printf("#<unbound>");
    }
    else if (a == Scm_Unspecified) {
        printf("#<unspecified>");
    }
    else if (a == Scm_Nil) {
        printf("()");
    }
    else if (Scm_HeapObjType(a) == Scm_ClosureType) {
        printf("#<closure %s@%p>", ScmClosure_Name(a), (void *) a);
    }
    else if (Scm_HeapObjType(a) == Scm_PairType) {
        printf("(");
        Scm_RawDisplay(ScmPair_Car(a));
        a = ScmPair_Cdr(a);
        while (Scm_IsPointer(a) && Scm_HeapObjType(a) == Scm_PairType) {
            printf(" ");
            Scm_RawDisplay(ScmPair_Car(a));
            a = ScmPair_Cdr(a);
        }
        if (a != Scm_Nil) {
            printf(" . ");
            Scm_RawDisplay(a);
        }
        printf(")");
    }
    else {
        printf("#<unknown %p>", (void *) a);
    }
}

static void
obj_display(ScmPtr _, ScmPtr a, ScmPtr k) {
    Scm_RawDisplay(a);
    Scm_TailCall1(k, Scm_Unspecified);
}
static ScmClosure _display = Scm_MkSuperComb(obj_display, "display", 2);
ScmPtr display = (ScmPtr) &_display;

static void
show_newline(ScmPtr _, ScmPtr k) {
    printf("\n");
    Scm_TailCall1(k, Scm_Unspecified);
}
static ScmClosure _newline = Scm_MkSuperComb(show_newline, "newline", 1);
ScmPtr newline = (ScmPtr) &_newline;

static void
int_add(ScmPtr _, ScmPtr a, ScmPtr b, ScmPtr k) {
    Scm_Assert(!Scm_IsPointer(a) &&
               Scm_IsInt(a) &&
               !Scm_IsPointer(b) &&
               Scm_IsInt(b));
    ScmPtr res = Scm_MkInt(Scm_GetInt(a) + Scm_GetInt(b));
    Scm_TailCall1(k, res);
}
static ScmClosure _zg = Scm_MkSuperComb(int_add, "+", 3);
ScmPtr zg = (ScmPtr) &_zg;  /* + */

static void
int_sub(ScmPtr _, ScmPtr a, ScmPtr b, ScmPtr k) {
    Scm_Assert(!Scm_IsPointer(a) &&
               Scm_IsInt(a) &&
               !Scm_IsPointer(b) &&
               Scm_IsInt(b));
    ScmPtr res = Scm_MkInt(Scm_GetInt(a) - Scm_GetInt(b));
    Scm_TailCall1(k, res);
}
static ScmClosure _zh = Scm_MkSuperComb(int_sub, "-", 3);
ScmPtr zh = (ScmPtr) &_zh;  /* - */

static void
int_lt(ScmPtr _, ScmPtr a, ScmPtr b, ScmPtr k) {
    Scm_Assert(!Scm_IsPointer(a) &&
               Scm_IsInt(a) &&
               !Scm_IsPointer(b) &&
               Scm_IsInt(b));
    ScmPtr res = Scm_GetInt(a) < Scm_GetInt(b) ? Scm_True : Scm_False;
    Scm_TailCall1(k, res);
}
static ScmClosure _zl = Scm_MkSuperComb(int_lt, "<", 3);
ScmPtr zl = (ScmPtr) &_zl;  /* < */

static void
halt_func(ScmPtr _, ScmPtr a) {
#ifdef SCM_GC_STAT
    Scm_GcSummary();
#endif
    Scm_GcFini();
    exit(0);
}
static ScmClosure _halt = Scm_MkSuperComb(halt_func, "halt", 1);
ScmPtr halt = (ScmPtr) &_halt;

static void
mk_cons(ScmPtr _, ScmPtr a, ScmPtr d, ScmPtr k) {
    ScmPtr newPair;
    newPair = Scm_GcAlloc(sizeof(ScmPair));
    if (!newPair) {
        Scm_PushGcRoot(a);
        Scm_PushGcRoot(d);
        Scm_PushGcRoot(k);
        Scm_GcCollect();
        k = Scm_PopGcRoot();
        d = Scm_PopGcRoot();
        a = Scm_PopGcRoot();
        newPair = Scm_GcAlloc(sizeof(ScmPair));
    }
    Scm_Assert(newPair);
    Scm_HeapObjType(newPair) = Scm_PairType;
    ScmPair_Car(newPair) = a;
    ScmPair_Cdr(newPair) = d;
    Scm_TailCall1(k, newPair);
}
static ScmClosure _cons = Scm_MkSuperComb(mk_cons, "cons", 3);
ScmPtr cons = (ScmPtr) &_cons;

static void
get_car(ScmPtr _, ScmPtr thePair, ScmPtr k) {
    Scm_Assert(Scm_IsPointer(thePair) &&
               Scm_HeapObjType(thePair) == Scm_PairType);
    Scm_TailCall1(k, ScmPair_Car(thePair));
}
static ScmClosure _get_car = Scm_MkSuperComb(get_car, "get-car", 2);
ScmPtr car = (ScmPtr) &_get_car;

static void
get_cdr(ScmPtr _, ScmPtr thePair, ScmPtr k) {
    Scm_Assert(Scm_IsPointer(thePair) &&
               Scm_HeapObjType(thePair) == Scm_PairType);
    Scm_TailCall1(k, ScmPair_Cdr(thePair));
}
static ScmClosure _get_cdr = Scm_MkSuperComb(get_cdr, "get-cdr", 2);
ScmPtr cdr = (ScmPtr) &_get_cdr;

static void
set_car(ScmPtr _, ScmPtr thePair, ScmPtr newCar, ScmPtr k) {
    Scm_Assert(Scm_IsPointer(thePair) &&
               Scm_HeapObjType(thePair) == Scm_PairType);
    Scm_GcWriteBarrier(thePair, newCar);
    ScmPair_Car(thePair) = newCar;
    Scm_TailCall1(k, Scm_Unspecified);
}
static ScmClosure _set_car = Scm_MkSuperComb(set_car, "set-car!", 3);
ScmPtr setzhcarza = (ScmPtr) &_set_car;

static void
set_cdr(ScmPtr _, ScmPtr thePair, ScmPtr newCdr, ScmPtr k) {
    Scm_Assert(Scm_IsPointer(thePair) &&
               Scm_HeapObjType(thePair) == Scm_PairType);
    Scm_GcWriteBarrier(thePair, newCdr);
    ScmPair_Cdr(thePair) = newCdr;
    Scm_TailCall1(k, Scm_Unspecified);
}
static ScmClosure _set_cdr = Scm_MkSuperComb(set_cdr, "set-cdr!", 3);
ScmPtr setzhcdrza = (ScmPtr) &_set_cdr;

static void
enter_cont(ScmPtr thisClosure, ScmPtr value, ScmPtr _) {
    /* abandon the current continuation, jump to the saved continuation */
    Scm_TailCall1(ScmClosure_GetUpvalAt(thisClosure, 0), value);
}

static void
prim_call_cc(ScmPtr _, ScmPtr clo, ScmPtr k) {
    ScmPtr newCont;
    newCont = Scm_GcAlloc(sizeof(ScmClosure) + 8);
    if (!newCont) {
        Scm_PushGcRoot(clo);
        Scm_PushGcRoot(k);
        Scm_GcCollect();
        k = Scm_PopGcRoot();
        clo = Scm_PopGcRoot();
        newCont = Scm_GcAlloc(sizeof(ScmClosure) + 8);
        Scm_Assert(newCont);
    }
    Scm_HeapObjType(newCont) = Scm_ClosureType;
    ScmClosure_Code(newCont) = enter_cont;
    ScmClosure_SetUpvalAt(newCont, 0, k);
    ScmClosure_Name(newCont) = "continuation";
    ScmClosure_Arity(newCont) = 2;
    Scm_TailCall2(clo, newCont, k);
}
static ScmClosure _call_cc = Scm_MkSuperComb(prim_call_cc, "call/cc", 2);
ScmPtr callzjcc = (ScmPtr) &_call_cc;

static void
ptr_eq(ScmPtr _, ScmPtr lhs, ScmPtr rhs, ScmPtr k) {
    ScmPtr res = lhs == rhs ? Scm_True : Scm_False;
    Scm_TailCall1(k, res);
}
static ScmClosure _ptr_eq = Scm_MkSuperComb(ptr_eq, "eq?", 3);
ScmPtr eqzo = (ScmPtr) &_ptr_eq;

