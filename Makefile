
examples = charx hello goodbye

beebasm_bytes = $(patsubst %, _build/%-beebasm.bytes, $(examples))
haskell_bytes = $(patsubst %, _build/%-haskell.bytes, $(examples))

top: diff

all: $(beebasm_bytes) $(haskell_bytes)

diff: $(patsubst %, diff-%, $(examples))

diff-%: _build/%-beebasm.bytes _build/%-haskell.bytes
	@ echo $@
	@ ./hexdiff.sh $^

run-%: _build/%-haskell.ssd
	b-em $<

ref-run-%: _build/%-beebasm.ssd
	b-em $<

_build/%.ssd: _build/%.bytes wrap.asm Makefile
	@ echo 'Wrapping $< as disc-image: $@'
	@ beebasm -S BinFile=$< -i wrap.asm -do $@ -boot Code || rm $@

_build/%-beebasm.bytes: asm/%.asm Makefile
	@ echo 'Compiling $< (using beebasm) --> $@'
	@ beebasm -i $< -o $@ || rm $@

_build/%-haskell.bytes: src/*.hs Makefile
	@ echo 'Generating (using Haskell) --> $@'
	@ stack run -- $@

