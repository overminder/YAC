#include "scm_runtime.h"

static void obj_display(ScmPtr _, ScmPtr a, ScmPtr k) {
    if ((a & 0x3) == 1) {
        printf("%ld", a >> 2);
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
    else {
        printf("#<unknown %p>", (void *) a);
    }
    Scm_TailCall1(k, Scm_Unspecified);
}
static ScmClosure _display = Scm_MkSuperComb(obj_display);
ScmPtr display = (ScmPtr) &_display;

static void int_add(ScmPtr _, ScmPtr a, ScmPtr b, ScmPtr k) {
    ScmPtr res = Scm_MkInt((a >> 2) + (b >> 2));
    Scm_TailCall1(k, res);
}
static ScmClosure _zg = Scm_MkSuperComb(int_add);
ScmPtr zg = (ScmPtr) &_zg;  /* + */

static void int_sub(ScmPtr _, ScmPtr a, ScmPtr b, ScmPtr k) {
    ScmPtr res = Scm_MkInt((a >> 2) - (b >> 2));
    Scm_TailCall1(k, res);
}
static ScmClosure _zh = Scm_MkSuperComb(int_sub);
ScmPtr zh = (ScmPtr) &_zh;  /* - */

static void int_lt(ScmPtr _, ScmPtr a, ScmPtr b, ScmPtr k) {
    ScmPtr res = (a >> 2) < (b >> 2) ? Scm_True : Scm_False;
    Scm_TailCall1(k, res);
}
static ScmClosure _zl = Scm_MkSuperComb(int_lt);
ScmPtr zl = (ScmPtr) &_zl;  /* < */

static void halt_func(ScmPtr _, ScmPtr a) {
    Scm_GcFini();
    exit(0);
}
static ScmClosure _halt = Scm_MkSuperComb(halt_func);
ScmPtr halt = (ScmPtr) &_halt;

// XXXX XX10
ScmPtr Scm_True = 2;
ScmPtr Scm_False = 6;
ScmPtr Scm_Unspecified = 10;
ScmPtr Scm_Unbound = 14;

