
examples = $(patsubst example/%.hs, %, $(wildcard example/*.hs))
asm_examples = $(patsubst asm/%.asm, %, $(wildcard asm/*.asm))

beebasm_bytes = $(patsubst %, _build/%-beebasm.bytes, $(asm_examples))
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

exe = .stack-work/dist/x86_64-linux/Cabal-3.6.3.0/build/main.exe/main.exe

_build/%-haskell.bytes: $(exe) Makefile
	@ echo 'Generating (using Haskell) --> $@'
	@ $(exe) $* $@

$(exe): src/*.hs example/*.hs Makefile
	stack build || rm $(exe)
