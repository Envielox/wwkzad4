CC=gcc
CFLAGS=--std=gnu99 -g -Wall

ALL: driver

driver: driver.c copilot-c99-codegen/copilot.o

copilot-c99-codegen/copilot.c: Paxos.hs
	rm -rf copilot-c99-codegen
	runhaskell GenerateCode.hs

clean:
	rm -f *.o
	rm -rf copilot-c99-codegen
