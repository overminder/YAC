X64 calling convention
----------------------
Is shown below::

    pre-arg-push:
    | 0xffff                 0 |
    | ----- stack growth ----> |
    [locals..., space-for-args]
    |                        ^rsp
  
    after-arg-push and pre-call (a1-a6 are passed in reg):
    | 0xffff                 0 |
    | ----- stack growth ----> |
    [a9:16(rsp), a8:8(rsp), a7:0(rsp)]
  
    after-call and pre-callee-save:
    | 0xffff                 0 |
    | ----- stack growth ----> |
    [a9:24(rsp), a8:16(rsp), a7:8(rsp), retAddr:0(rsp)]
  
    after-callee-save after-new-rbp
    | where N = all pushes (including push rbp)
    | 0xffff                 0 |
    | ----- stack growth ----> |
    [a9:8N+24(rbp), a8:8N+16(rbp), a7:8N+8(rbp), retAddr:8N(rbp), ...xN]
  
  
