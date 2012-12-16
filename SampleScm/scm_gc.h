#ifndef SCM_GC_H
#define SCM_GC_H

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

void Scm_GcInit(void);
void Scm_GcFini(void);
void Scm_AddGlobalGcRoot(ScmPtr *);
void Scm_GcCollect(void);
void Scm_GcSummary(void);

#endif  /* SCM_GC_H */
