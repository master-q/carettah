SRCS := $(wildcard *.hs)

all: carettah

carettah: Carettah.hs $(SRCS)
	ghc --make -Wall -idata Carettah.hs -o carettah

lint:
	hlint -c $(SRCS)

clean:
	rm -rf carettah
	rm -rf *.hi *.o *~
	cd data/ && rm -rf *.hi *.o *~

.PHONY: lint clean
