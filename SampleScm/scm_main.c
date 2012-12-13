#include "scm_runtime.h"

int
main(int argc, char **argv) {
    Scm_GcInit();
    Scm_TailCall1(Mainzkmain, halt);
}

