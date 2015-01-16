.PHONY: all
all: blocking
	blocking -t *.touch
	blocking \
	  clarion-iup.match \
	  tampa-wju-2014.match \
	  tampa-fortsmith-2014.match \
	  tampa-sw-minn-2014.match \
	  penn-stanford-2014.match \
	  penn-byu-2014.match \
	  hawaii-stanford-1987.match \
	  japan-ussr-1964.match

Match.hs: Match.y
	happy Match.y

Touches.hs: Touches.y
	happy Touches.y

blocking: Blocking.hs Match.hs Touches.hs
	ghc --make -W -o blocking Blocking.hs

.PHONY: clean
clean:
	-rm blocking
	-rm *.o *.hi
	-rm Match.hs
	-rm Touches.hs

