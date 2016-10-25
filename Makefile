SRCS := $(wildcard *.hs)

all:
	cabal build

install:
	cabal install

lint:
	hlint -c $(SRCS)

clean:
	cabal clean

.PHONY: all install lint clean
