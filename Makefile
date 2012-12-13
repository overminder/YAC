srcs = $(shell find -name "*.hs")
outdir = bin
ir2asm = IRToAsm
scm2ir = ScmToIR
ghcflags = -O1 --make

all : $(outdir)/$(ir2asm) $(outdir)/$(scm2ir)

$(outdir):
	mkdir $(outdir)

$(outdir)/$(ir2asm) : $(srcs) $(outdir)
	ghc $(ghcflags) $(ir2asm).hs -o $(outdir)/$(ir2asm)

$(outdir)/$(scm2ir) : $(srcs) $(outdir)
	ghc $(ghcflags) $(scm2ir).hs -o $(outdir)/$(scm2ir)

clean :
	find -name "*.o" -delete
	rm $(outdir) -rf
.PHONY : clean

