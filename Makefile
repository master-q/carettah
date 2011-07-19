all: carettah

carettah: Carettah.hs
	ghc --make -threaded -Wall Carettah.hs -o carettah

lint: Carettah.hs
	hlint -c Carettah.hs

clean:
	rm -rf 
	rm -rf *.hi *.o
	rm -rf *~

.PHONY: lint clean
