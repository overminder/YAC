srcs = $(shell find ../../ -name "*.hs")
outdir = bin
test = Main
ghcflags = -O1 --make -i../../

all : $(outdir)/$(test)

test : $(outdir)/$(test)
	$(outdir)/$(test)

$(outdir)/$(test) : $(srcs) $(outdir)
	ghc $(ghcflags) $(test).hs -o $(outdir)/$(test)

$(outdir) :
	mkdir $(outdir)

clean :
	find -name "*.o" -delete
	find -name "*.hi" -delete
	rm $(outdir) -r
.PHONY : clean

