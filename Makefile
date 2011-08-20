all: carettah

carettah: Carettah.hs
	ghc --make -Wall Carettah.hs -o carettah

lint: Carettah.hs Config.hs Render.hs
	hlint -c Carettah.hs Config.hs Render.hs

clean:
	rm -rf carettah
	rm -rf *.hi *.o
	rm -rf *~

.PHONY: lint clean
