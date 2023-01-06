.PHONY: build clean

# Quat13.hs
# Quat.hs
build:
	ghc -o deCasteljau deCasteljau.hs
	ghc -o Test Test.hs

clean:
	rm -f *.hi *.o deCasteljau Test


