all: copilot.c
	wc -c copilot.c

copilot.c: Example.hs Demands.hs State.hs Motors.hs Mixers.hs AltitudeHold.hs
	runhaskell Example.hs
	wc -c copilot.c

edit:
	vim Example.hs

clean:
	rm -f copilot.* tmp.c *~
