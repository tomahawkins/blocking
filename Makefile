.PHONY: all
all: blocking
	blocking tampa-sw-minn-2014.match clarion-iup.match tampa-wju-2014.match

Match.hs: Match.y
	happy Match.y

blocking: Blocking.hs Match.hs
	ghc --make -W -o blocking Blocking.hs

.PHONY: clean
clean:
	-rm blocking
	-rm *.o *.hi
	-rm Match.hs

