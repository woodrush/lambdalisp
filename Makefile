# This setting needs to be changed on a Mac to compile tromp.c (make ./bin/tromp, make test-blc-tromp, etc.).
# Please see README.md for details.
CC=cc

BLC=./bin/Blc
UNI=./bin/uni
TROMP=./bin/tromp
ULAMB=./bin/clamb
LAZYK=./bin/lazyk
SBCL=sbcl

ASC2BIN=./bin/asc2bin

target_blc=lambdalisp.blc
target_ulamb=lambdalisp.ulamb
target_lazy=lambdalisp.lazy
target_latex=lambdalisp.tex

def_prelude=./src/build/def-prelude.cl
def_prelude_lazyk=./src/build/def-prelude-lazyk.cl

BASE_SRCS=./src/lambdalisp.cl ./src/lambdacraft.cl ./src/prelude.lisp


all:
	$(MAKE) $(target_blc)
	$(MAKE) $(target_ulamb)

test: test-blc-uni test-compiler-hosting-blc-uni
test-all: test-blc-uni test-ulamb test-lazyk test-compiler-hosting-blc-uni test-blc-tromp

# On x86-64-Linux, the interpreter 'Blc' can be used.
test-linux: test-blc test-blc-tromp test-compiler-hosting-blc test-compiler-hosting-blc-uni
test-all-linux: test-blc test-blc-tromp test-blc-uni test-ulamb test-lazyk test-compiler-hosting-blc test-compiler-hosting-blc-uni

interpreters: $(UNI) $(ULAMB) $(LAZYK) $(TROMP) $(BLC)



#================================================================
# Tests
#================================================================
# SBCL comparison test - compare LambdaLisp outputs with Common Lisp outputs for examples/*.cl
.PRECIOUS: out/%.cl.sbcl-out
out/%.cl.sbcl-out: examples/%.cl
	mkdir -p ./out
	$(SBCL) --script $< > $@.tmp
	mv $@.tmp $@

.PRECIOUS: out/%.blc-out
out/%.blc-out: examples/% $(target_blc) $(BLC) $(ASC2BIN)
	mkdir -p ./out
	( cat $(target_blc) | $(ASC2BIN); cat $< ) | $(BLC) > $@.tmp
	mv $@.tmp $@

.PRECIOUS: out/%.blc-tromp-out
out/%.blc-tromp-out: examples/% $(target_blc) $(TROMP) $(ASC2BIN)
	mkdir -p ./out
	( cat $(target_blc) | $(ASC2BIN); cat $< ) | $(TROMP) > $@.tmp
	mv $@.tmp $@

.PRECIOUS: out/%.blc-uni-out
out/%.blc-uni-out: examples/% $(target_blc) $(UNI) $(ASC2BIN)
	mkdir -p ./out
	( cat $(target_blc) | $(ASC2BIN); cat $< ) | $(UNI) > $@.tmp
	mv $@.tmp $@

.PRECIOUS: out/%.ulamb-out
out/%.ulamb-out: examples/% $(target_ulamb) $(ULAMB) $(ASC2BIN)
	mkdir -p ./out
	( cat $(target_ulamb) | $(ASC2BIN); cat $< ) | $(ULAMB) > $@.tmp
	mv $@.tmp $@

.PRECIOUS: out/%.lazyk-out
out/%.lazyk-out: examples/% $(target_lazy) $(LAZYK)
	mkdir -p ./out
	cat $< | $(LAZYK) $(target_lazy) -u > $@.tmp
	mv $@.tmp $@

out/%.cleaned: out/%
# Remove the initial '> ' printed by LambdaLisp's REPL when comparing with SBCL's output
	cat $< | sed -e '1s/> //' > $<.cleaned

out/%.blc-out.sbcl-diff: ./out/%.blc-out.cleaned ./out/%.sbcl-out
	cmp $^ || exit 1

out/%.blc-tromp-out.sbcl-diff: ./out/%.blc-tromp-out.cleaned ./out/%.sbcl-out
	cmp $^ || exit 1

out/%.blc-uni-out.sbcl-diff: ./out/%.blc-uni-out.cleaned ./out/%.sbcl-out
	cmp $^ || exit 1

out/%.ulamb-out.sbcl-diff: ./out/%.ulamb-out.cleaned ./out/%.sbcl-out
	cmp $^ || exit 1

out/%.lazyk-out.sbcl-diff: ./out/%.lazyk-out.cleaned ./out/%.sbcl-out
	cmp $^ || exit 1

.PHONY: test-blc
test-blc: $(addsuffix .blc-out.sbcl-diff, $(addprefix out/, $(notdir $(wildcard examples/*.cl))))
	@echo "All tests have passed for BLC with the interpreter 'Blc'."

.PHONY: test-blc-tromp
test-blc-tromp: $(addsuffix .blc-tromp-out.sbcl-diff, $(addprefix out/, $(notdir $(wildcard examples/*.cl))))
	@echo "All tests have passed for BLC with the interpreter 'tromp'."

.PHONY: test-blc-uni
test-blc-uni: $(addsuffix .blc-uni-out.sbcl-diff, $(addprefix out/, $(notdir $(wildcard examples/*.cl))))
	@echo "All tests have passed for BLC with the interpreter 'uni'."

.PHONY: test-ulamb
test-ulamb: $(addsuffix .ulamb-out.sbcl-diff, $(addprefix out/, $(notdir $(wildcard examples/*.cl))))
	@echo "All tests have passed for Universal Lambda."

.PHONY: test-lazyk
test-lazyk: $(addsuffix .lazyk-out.sbcl-diff, $(addprefix out/, $(notdir $(wildcard examples/*.cl))))
	@echo "All tests have passed for Lazy K."


# Compiler hosting test - execute the output of examples/lambdacraft.cl as a binary lambda calculus program
.PHONY: test-compiler-hosting-blc
test-compiler-hosting-blc: out/lambdacraft.cl.blc-out $(BLC) $(ASC2BIN) examples/lambdacraft.cl
# Remove non-01-characters and provide it to BLC
	cat $< | sed 's/[^0-9]*//g' | tr -d "\n" | $(ASC2BIN) | $(BLC) > $@
	printf 'A' > out/lambdacraft.cl.blc-expected
	cmp $@ out/lambdacraft.cl.blc-expected || ( rm $@; exit 1)
	@echo "LambdaCraft-compiler-hosting-on-LambdaLisp test passed."

.PHONY: test-compiler-hosting-blc-uni
test-compiler-hosting-blc-uni: out/lambdacraft.cl.blc-uni-out $(UNI) $(ASC2BIN) examples/lambdacraft.cl
# Remove non-01-characters and provide it to BLC
	cat $< | sed 's/[^0-9]*//g' | tr -d "\n" | $(ASC2BIN) | $(UNI) > $@
	printf 'A' > out/lambdacraft.cl.blc-expected
	cmp $@ out/lambdacraft.cl.blc-expected || ( rm $@; exit 1)
	@echo "LambdaCraft-compiler-hosting-on-LambdaLisp test passed."


# Self-hosting test - compile LambdaLisp's own source code written in Common Lisp using the LambdaLisp interpreter (currently theoretical - requires a lot of time and memory)
.PHONY: test-self-host
test-self-host: $(BASE_SRCS) $(def_prelude) ./src/main.cl $(target_blc) $(BLC) $(ASC2BIN)
	mkdir -p ./test
	( echo '(defparameter **lambdalisp-suppress-repl** t)'; \
	  cat ./src/lambdacraft.cl ./src/build/def-prelude.cl ./src/lambdalisp.cl ./src/main.cl ) > ./out/lambdalisp-src-cat.cl
	( cat $(target_blc) | $(ASC2BIN); cat ./out/lambdalisp-src-cat.cl ) | $(BLC) > ./out/lambdalisp.cl.blc.repl.out
	cat ./out/lambdalisp.cl.blc.repl.out | sed -e '1s/> //' | ./out/lambdalisp.cl.blc.script.out
	cmp ./out/lambdalisp.cl.blc.script.out $(target_blc) || (echo "The LambdaLisp output does not match with the Common Lisp output." && exit 1)
	@echo "LambdaLisp self-hosting test passed."



#================================================================
# Building the source
#================================================================
# Compile the prelude
$(def_prelude): ./src/prelude.lisp ./tools/compile-prelude.sh
	mkdir -p src/build
	./tools/compile-prelude.sh > $(def_prelude).tmp
	mv $(def_prelude).tmp $(def_prelude)

$(def_prelude_lazyk): ./src/prelude.lisp ./tools/compile-prelude.sh
	mkdir -p src/build
	./tools/compile-prelude.sh compile-lazyk > $(def_prelude_lazyk).tmp
	mv $(def_prelude_lazyk).tmp $(def_prelude_lazyk)


# Compile the main code
blc: $(target_blc)
$(target_blc): $(BASE_SRCS) $(def_prelude) ./src/main.cl
	cd src; sbcl --script main.cl > ../$(target_blc).tmp
	mv $(target_blc).tmp $(target_blc)

ulamb: $(target_ulamb)
$(target_ulamb): $(BASE_SRCS) $(def_prelude) ./src/main-ulamb.cl ./src/lazyk-ulamb-blc-wrapper.cl
	cd src; sbcl --script ./main-ulamb.cl > ../$(target_ulamb).tmp
	mv $(target_ulamb).tmp $(target_ulamb)

lazyk: $(target_lazy)
$(target_lazy): $(BASE_SRCS) $(def_prelude_lazyk) ./src/lazyk-chars.cl ./src/main-lazyk.cl ./src/lazyk-ulamb-blc-wrapper.cl
	@echo "Compiling to Lazy K takes a while (several minutes)."
	cd src; sbcl --script ./main-lazyk.cl > ../$(target_lazy).tmp

	# Replace ``s`kki with k, which are equivalent terms
	cat $(target_lazy).tmp | sed s/\`\`s\`kki/k/g > $(target_lazy).tmp2
	mv $(target_lazy).tmp2 $(target_lazy)
	rm $(target_lazy).tmp


# Additional targets
latex: $(target_latex)
$(target_latex): $(BASE_SRCS) $(def_prelude) ./src/main-latex.cl
	./tools/make-latex.sh



#================================================================
# Building the interpreters
#================================================================
./build/clamb/clamb.c:
	mkdir -p ./build
	cd build; git clone https://github.com/irori/clamb

$(ULAMB): ./build/clamb/clamb.c
	mkdir -p ./bin
	cd build/clamb; $(CC) -O2 clamb.c -o clamb
	mv build/clamb/clamb ./bin
	chmod 755 $(ULAMB)


./build/lazyk/lazyk.c:
	mkdir -p ./build
	cd build; git clone https://github.com/irori/lazyk

$(LAZYK): ./build/lazyk/lazyk.c
	mkdir -p ./bin
	cd build/lazyk; $(CC) -O2 lazyk.c -o lazyk
	mv build/lazyk/lazyk ./bin
	chmod 755 $(LAZYK)


.PHONY: show_Blc.S_message
show_Blc.S_message:
	@echo
	@echo "    This procedure requires the binary lambda calculus interpreter 'Blc'."
	@echo "    To compile it and proceed, please place Blc.S and flat.lds under ./build."
	@echo "    (Please use the uppercase Blc.S, and not the lowercase blc.S.)"
	@echo "    Blc can be run on x86-64 Linux systems. In other environments, ./bin/tromp or ./bin/uni (make test-blc-tromp, make test-blc-uni) can be used."
	@echo "    Please see README.md for details."
	@echo
	@exit 1

build/Blc.S:
	mkdir -p ./build
	if [ ! -f $@ ]; then $(MAKE) show_Blc.S_message; fi

build/flat.lds:
	mkdir -p ./build
	if [ ! -f $@ ]; then $(MAKE) show_Blc.S_message; fi

$(BLC): build/Blc.S build/flat.lds
	mkdir -p ./bin
	# Extend the maximum memory limit to execute large programs
	cd build; cat Blc.S | sed -e 's/#define TERMS	5000000/#define TERMS	50000000/' > Blc.ext.S
	cd build; $(CC) -c -o Blc.o Blc.ext.S
	cd build; ld.bfd -o Blc Blc.o -T flat.lds
	mv build/Blc ./bin
	chmod 755 $(BLC)


.PHONY: show_tromp.c_message
show_tromp.c_message:
	@echo
	@echo "    This procedure requires the binary lambda calculus interpreter 'tromp'."
	@echo "    To compile it and proceed, please place tromp.c under ./build."
	@echo "    Note that on a Mac, the default gcc may not compile tromp.c, and an installation of a different version for gcc may be required."
	@echo "    Please see README.md for details."
	@echo
	@exit 1

build/tromp.c:
	mkdir -p ./build
	if [ ! -f $@ ]; then $(MAKE) show_tromp.c_message; fi

$(TROMP): ./build/tromp.c
	mkdir -p ./bin
	# Compile with the option -DA=9999999 (larger than the original -DM=999999) to execute large programs
	cd build; $(CC) -Wall -W -std=c99 -O2 -m64 -DInt=long -DA=9999999 -DX=8 tromp.c -o tromp
	mv build/tromp ./bin
	chmod 755 $(TROMP)


.PHONY: show_uni.c_message
show_uni.c_message:
	@echo
	@echo "    This procedure requires the binary lambda calculus interpreter 'uni'."
	@echo "    To compile it and proceed, please place uni.c under ./build."
	@echo "    Please see README.md for details."
	@echo
	@exit 1

build/uni.c:
	mkdir -p ./build
	if [ ! -f $@ ]; then $(MAKE) show_uni.c_message; fi

$(UNI): ./build/uni.c
	mkdir -p ./bin
	# Compile with the option -DA=9999999 (larger than the original -DM=999999) to execute large programs
	cd build; $(CC) -Wall -W -O2 -std=c99 -m64 -DM=9999999 uni.c -o uni
	mv build/uni ./bin
	chmod 755 $(UNI)


$(ASC2BIN): ./tools/asc2bin.c
	mkdir -p ./bin
	cd build; $(CC) ../tools/asc2bin.c -O2 -o asc2bin
	mv build/asc2bin ./bin
	chmod 755 $(ASC2BIN)
