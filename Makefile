all: $(basename $(wildcard *.hs))

%: %.hs
	ghc --make -o $@ $<
