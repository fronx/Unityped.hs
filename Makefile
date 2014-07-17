.PHONY: run clean

all:
	ghc -prof -fprof-auto -main-is Unityped.main Unityped.hs

run:
	./Unityped

clean:
	rm -f Unityped
	rm -f Unityped.o
	rm -f Unityped.hi
