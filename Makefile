srcs = $(shell find -name "*.hs")
outdir = bin
ir2asm = IRToAsm
scm2ir = ScmToIR
ghcflags = -O2 -dynamic --make

all : $(outdir)/$(ir2asm) $(outdir)/$(scm2ir)

$(outdir)/$(ir2asm) : $(srcs)
	ghc $(ghcflags) $(ir2asm).hs -o $(outdir)/$(ir2asm)

$(outdir)/$(scm2ir) : $(srcs)
	ghc $(ghcflags) $(scm2ir).hs -o $(outdir)/$(scm2ir)

clean :
	find -name "*.o" -delete
	rm $(outdir)/$(ir2asm) $(outdir)/$(scm2ir) -f
.PHONY : clean

