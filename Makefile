
top: build-all

run: run-hello

run-%: _build/%.ssd
	b-em $<

units = $(patsubst asm/%.asm, %, $(wildcard asm/*.asm))
ssds = $(patsubst %, _build/%.ssd, $(units))

build-all: _build $(ssds)

_build/%.ssd: _build/%.bytes wrap.asm Makefile
	@ echo Wrapping as disc-image: $@
	@ beebasm -S BinFile=$< -i wrap.asm -do $@ -boot Code || rm $@

_build/%.bytes: asm/%.asm Makefile
	@ echo Compiling $<
	@ beebasm -i $< -o $@ || rm $@
