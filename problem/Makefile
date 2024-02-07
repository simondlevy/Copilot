all: copilot.c
	wc -c copilot.c

copilot.c: *.hs
	runhaskell Example.hs
	wc -c copilot.c

edit:
	vim Example.hs

clean:
	rm -f *.c *.h
