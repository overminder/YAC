outdir = "bin"
ir2asm = "IRToAsm"
scm2ir = "ScmToIR"

all : $(ir2asm) $(scm2ir)

$(ir2asm) : $(find -name "*.hs")
	ghc --make $(ir2asm).hs -o $(outdir)/$(ir2asm)

$(scm2ir) : $(find -name "*.hs")
	ghc --make $(scm2ir).hs -o $(outdir)/$(scm2ir)

