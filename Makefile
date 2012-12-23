srcs = $(shell find -name "*.hs")
outdir = bin
sexpr2asm = SExprToAsm
ca2asm = CaToAsm
scm2ir = ScmToIR
ghcflags = -O1 --make

all : $(outdir)/$(sexpr2asm) $(outdir)/$(scm2ir) $(outdir)/$(ca2asm)

$(outdir):
	mkdir $(outdir)

$(outdir)/$(sexpr2asm) : $(srcs) $(outdir)
	ghc $(ghcflags) $(sexpr2asm).hs -o $(outdir)/$(sexpr2asm)

$(outdir)/$(ca2asm) : $(srcs) $(outdir)
	ghc $(ghcflags) $(ca2asm).hs -o $(outdir)/$(ca2asm)

$(outdir)/$(scm2ir) : $(srcs) $(outdir)
	ghc $(ghcflags) $(scm2ir).hs -o $(outdir)/$(scm2ir)

clean :
	find -name "*.o" -delete
	find -name "*.hi" -delete
	rm $(outdir) -rf
.PHONY : clean

