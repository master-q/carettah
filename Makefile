SRCS := $(wildcard *.hs)

all: carettah

carettah: Carettah.hs
	ghc --make -Wall Carettah.hs -o carettah

lint:
	hlint -c $(SRCS)

clean:
	rm -rf carettah
	rm -rf *.hi *.o
	rm -rf *~

.PHONY: lint clean
