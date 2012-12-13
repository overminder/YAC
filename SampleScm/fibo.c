/*
Main:main = -> {
  (display (fibo 10))}


fibo = \n -> {
  If (< n 2)
    Then {n}
    Else {(+ (fibo (- n 1)) (fibo (- n 2)))}}


******************************************************************************
Main:main = \$ScCont_0 -> {
  (fibo 10 \$ApCont_1 -> {
    (display $ApCont_1 $ScCont_0)}
  )}


fibo = \n $ScCont_2 -> {
  (< n 2 \$IfCont_3 -> {
    If $IfCont_3
      Then {($ScCont_2 n)}
      Else {(- n 1 \$ApCont_7 -> {
        (fibo $ApCont_7 \$ApCont_4 -> {
          (- n 2 \$ApCont_6 -> {
            (fibo $ApCont_6 \$ApCont_5 -> {
              (+ $ApCont_4 $ApCont_5 $ScCont_2)}
            )}
          )}
        )}
      )}}
  )}


******************************************************************************
Main:main = \$ScCont_0 -> {
  (fibo 10 {$ScCont_0}\$ApCont_1 -> {
    (display $ApCont_1 $ScCont_0)}
  )}


fibo = \n $ScCont_2 -> {
  (< n 2 {$ScCont_2 n}\$IfCont_3 -> {
    If $IfCont_3
      Then {($ScCont_2 n)}
      Else {(- n 1 {$ScCont_2 n}\$ApCont_7 -> {
        (fibo $ApCont_7 {$ScCont_2 n}\$ApCont_4 -> {
          (- n 2 {$ApCont_4 $ScCont_2}\$ApCont_6 -> {
            (fibo $ApCont_6 {$ApCont_4 $ScCont_2}\$ApCont_5 -> {
              (+ $ApCont_4 $ApCont_5 $ScCont_2)}
            )}
          )}
        )}
      )}}
  )}


******************************************************************************
Mainzkmain = \zcScCont_0 -> {
  (fibo 10 {zcScCont_0}\zcApCont_1 -> {
    (display zcApCont_1 zcScCont_0)}
  )}


fibo = \n zcScCont_2 -> {
  (zl n 2 {zcScCont_2 n}\zcIfCont_3 -> {
    If zcIfCont_3
      Then {(zcScCont_2 n)}
      Else {(zh n 1 {zcScCont_2 n}\zcApCont_7 -> {
        (fibo zcApCont_7 {zcScCont_2 n}\zcApCont_4 -> {
          (zh n 2 {zcApCont_4 zcScCont_2}\zcApCont_6 -> {
            (fibo zcApCont_6 {zcApCont_4 zcScCont_2}\zcApCont_5 -> {
              (zg zcApCont_4 zcApCont_5 zcScCont_2)}
            )}
          )}
        )}
      )}}
  )}


******************************************************************************
*/
#include "scm_runtime.h"
void Lambda_18(ScmPtr thisClosure, ScmPtr zcApCont_5);
void Lambda_16(ScmPtr thisClosure, ScmPtr zcApCont_6);
void Lambda_14(ScmPtr thisClosure, ScmPtr zcApCont_4);
void Lambda_12(ScmPtr thisClosure, ScmPtr zcApCont_7);
void Lambda_10(ScmPtr thisClosure, ScmPtr zcIfCont_3);
void Sc_fibo(ScmPtr thisClosure, ScmPtr n, ScmPtr zcScCont_2);
void Lambda_8(ScmPtr thisClosure, ScmPtr zcApCont_1);
void Sc_Mainzkmain(ScmPtr thisClosure, ScmPtr zcScCont_0);

static ScmClosure _fibo = Scm_MkSuperComb(Sc_fibo);
ScmPtr fibo = (ScmPtr) &_fibo;
static ScmClosure _Mainzkmain = Scm_MkSuperComb(Sc_Mainzkmain);
ScmPtr Mainzkmain = (ScmPtr) &_Mainzkmain;

void Lambda_18(ScmPtr thisClosure, ScmPtr zcApCont_5) {
    Scm_TailCall3(zg, ScmClosure_UpvalAt(thisClosure, 0), zcApCont_5, ScmClosure_UpvalAt(thisClosure, 1));
}
void Lambda_16(ScmPtr thisClosure, ScmPtr zcApCont_6) {
    ScmPtr newClosure_19;
    newClosure_19 = Scm_GcAlloc(sizeof(ScmClosure) + 16);
    if (!newClosure_19) {
        Scm_PushGcRoot(thisClosure);
        Scm_PushGcRoot(zcApCont_6);
        Scm_GcCollect();
        zcApCont_6 = Scm_PopGcRoot();
        thisClosure = Scm_PopGcRoot();
        newClosure_19 = Scm_GcAlloc(sizeof(ScmClosure) + 16);
    }
    ScmClosure_Code(newClosure_19) = Lambda_18;
    Scm_HeapObjType(newClosure_19) = Scm_ClosureType;
    ScmClosure_UpvalAt(newClosure_19, 0) = ScmClosure_UpvalAt(thisClosure, 0);
    ScmClosure_UpvalAt(newClosure_19, 1) = ScmClosure_UpvalAt(thisClosure, 1);
    Scm_TailCall2(fibo, zcApCont_6, newClosure_19);
}
void Lambda_14(ScmPtr thisClosure, ScmPtr zcApCont_4) {
    ScmPtr newClosure_17;
    newClosure_17 = Scm_GcAlloc(sizeof(ScmClosure) + 16);
    if (!newClosure_17) {
        Scm_PushGcRoot(thisClosure);
        Scm_PushGcRoot(zcApCont_4);
        Scm_GcCollect();
        zcApCont_4 = Scm_PopGcRoot();
        thisClosure = Scm_PopGcRoot();
        newClosure_17 = Scm_GcAlloc(sizeof(ScmClosure) + 16);
    }
    ScmClosure_Code(newClosure_17) = Lambda_16;
    Scm_HeapObjType(newClosure_17) = Scm_ClosureType;
    ScmClosure_UpvalAt(newClosure_17, 0) = zcApCont_4;
    ScmClosure_UpvalAt(newClosure_17, 1) = ScmClosure_UpvalAt(thisClosure, 0);
    Scm_TailCall3(zh, ScmClosure_UpvalAt(thisClosure, 1), Scm_MkInt(2), newClosure_17);
}
void Lambda_12(ScmPtr thisClosure, ScmPtr zcApCont_7) {
    ScmPtr newClosure_15;
    newClosure_15 = Scm_GcAlloc(sizeof(ScmClosure) + 16);
    if (!newClosure_15) {
        Scm_PushGcRoot(thisClosure);
        Scm_PushGcRoot(zcApCont_7);
        Scm_GcCollect();
        zcApCont_7 = Scm_PopGcRoot();
        thisClosure = Scm_PopGcRoot();
        newClosure_15 = Scm_GcAlloc(sizeof(ScmClosure) + 16);
    }
    ScmClosure_Code(newClosure_15) = Lambda_14;
    Scm_HeapObjType(newClosure_15) = Scm_ClosureType;
    ScmClosure_UpvalAt(newClosure_15, 0) = ScmClosure_UpvalAt(thisClosure, 0);
    ScmClosure_UpvalAt(newClosure_15, 1) = ScmClosure_UpvalAt(thisClosure, 1);
    Scm_TailCall2(fibo, zcApCont_7, newClosure_15);
}
void Lambda_10(ScmPtr thisClosure, ScmPtr zcIfCont_3) {
    if (zcIfCont_3 == Scm_True) {
        Scm_TailCall1(ScmClosure_UpvalAt(thisClosure, 0), ScmClosure_UpvalAt(thisClosure, 1));
    }
    else {
        ScmPtr newClosure_13;
        newClosure_13 = Scm_GcAlloc(sizeof(ScmClosure) + 16);
        if (!newClosure_13) {
            Scm_PushGcRoot(thisClosure);
            Scm_PushGcRoot(zcIfCont_3);
            Scm_GcCollect();
            zcIfCont_3 = Scm_PopGcRoot();
            thisClosure = Scm_PopGcRoot();
            newClosure_13 = Scm_GcAlloc(sizeof(ScmClosure) + 16);
        }
        ScmClosure_Code(newClosure_13) = Lambda_12;
        Scm_HeapObjType(newClosure_13) = Scm_ClosureType;
        ScmClosure_UpvalAt(newClosure_13, 0) = ScmClosure_UpvalAt(thisClosure, 0);
        ScmClosure_UpvalAt(newClosure_13, 1) = ScmClosure_UpvalAt(thisClosure, 1);
        Scm_TailCall3(zh, ScmClosure_UpvalAt(thisClosure, 1), Scm_MkInt(1), newClosure_13);
    }
}
void Sc_fibo(ScmPtr thisClosure, ScmPtr n, ScmPtr zcScCont_2) {
    ScmPtr newClosure_11;
    newClosure_11 = Scm_GcAlloc(sizeof(ScmClosure) + 16);
    if (!newClosure_11) {
        Scm_PushGcRoot(thisClosure);
        Scm_PushGcRoot(n);
        Scm_PushGcRoot(zcScCont_2);
        Scm_GcCollect();
        zcScCont_2 = Scm_PopGcRoot();
        n = Scm_PopGcRoot();
        thisClosure = Scm_PopGcRoot();
        newClosure_11 = Scm_GcAlloc(sizeof(ScmClosure) + 16);
    }
    ScmClosure_Code(newClosure_11) = Lambda_10;
    Scm_HeapObjType(newClosure_11) = Scm_ClosureType;
    ScmClosure_UpvalAt(newClosure_11, 0) = zcScCont_2;
    ScmClosure_UpvalAt(newClosure_11, 1) = n;
    Scm_TailCall3(zl, n, Scm_MkInt(2), newClosure_11);
}
void Lambda_8(ScmPtr thisClosure, ScmPtr zcApCont_1) {
    Scm_TailCall2(display, zcApCont_1, ScmClosure_UpvalAt(thisClosure, 0));
}
void Sc_Mainzkmain(ScmPtr thisClosure, ScmPtr zcScCont_0) {
    ScmPtr newClosure_9;
    newClosure_9 = Scm_GcAlloc(sizeof(ScmClosure) + 8);
    if (!newClosure_9) {
        Scm_PushGcRoot(thisClosure);
        Scm_PushGcRoot(zcScCont_0);
        Scm_GcCollect();
        zcScCont_0 = Scm_PopGcRoot();
        thisClosure = Scm_PopGcRoot();
        newClosure_9 = Scm_GcAlloc(sizeof(ScmClosure) + 8);
    }
    ScmClosure_Code(newClosure_9) = Lambda_8;
    Scm_HeapObjType(newClosure_9) = Scm_ClosureType;
    ScmClosure_UpvalAt(newClosure_9, 0) = zcScCont_0;
    Scm_TailCall2(fibo, Scm_MkInt(30), newClosure_9);
}
