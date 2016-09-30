SRCS := $(wildcard *.hs)

all:
	stack build

install:
	stack install

lint:
	hlint -c $(SRCS)

clean:
	stack clean

.PHONY: all install lint clean
