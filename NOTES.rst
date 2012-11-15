X64 calling convention
----------------------
Is shown below::

    pre-arg-push:
    | 0xffff                 0 |
    | ----- stack growth ----> |
    [locals..., space-for-args]
    |                        ^rsp
  
    after-arg-push and pre-call:
    | 0xffff                 0 |
    | ----- stack growth ----> |
    [a2:16(rsp), a1:8(rsp), a0:0(rsp)]
  
    after-call and pre-callee-save:
    | 0xffff                 0 |
    | ----- stack growth ----> |
    [a2:24(rsp), a1:16(rsp), a0:8(rsp), retAddr:0(rsp)]
  
    after-callee-save after-new-rbp
    | where N = all pushes (including push rbp)
    | 0xffff                 0 |
    | ----- stack growth ----> |
    [a2:8N+24(rbp), a1:8N+16(rbp), a0:8N+8(rbp), retAddr:8N(rbp), ...xN]
  
  
