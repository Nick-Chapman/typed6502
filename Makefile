
top: build-all view-charx

view-charx:
	hd _build/hello.beebasm-bytes
	hd _build/hello.haskell-bytes

examples = charx hello

beebasm_bytes = $(patsubst %, _build/%.beebasm-bytes, $(examples))
haskell_bytes = $(patsubst %, _build/%.haskell-bytes, $(examples))

build-all: $(beebasm_bytes) $(haskell_bytes)

run-%: _build/%.ssd
	b-em $<

#Assembler=beebasm
Assembler=haskell

_build/%.ssd: _build/%.$(Assembler)-bytes wrap.asm Makefile
	@ echo 'Wrapping $< as disc-image: $@'
	@ beebasm -S BinFile=$< -i wrap.asm -do $@ -boot Code || rm $@

_build/%.beebasm-bytes: asm/%.asm Makefile
	@ echo 'Compiling $< (using beebasm) --> $@'
	@ beebasm -i $< -o $@ || rm $@

_build/%.haskell-bytes: src/*.hs Makefile
	@ echo 'Generating (using Haskell) --> $@'
	@ stack run -- $@

