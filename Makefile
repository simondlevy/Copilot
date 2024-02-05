all: copilot.c
	wc -c copilot.c

copilot.c: Example.hs
	runhaskell Example.hs
	wc -c copilot.c

clean:
	rm -f copilot.* tmp.c *~
