BLC=./bin/Blc
TROMP=./bin/tromp
ULAMB=./bin/clamb
LAZYK=./bin/lazyk
SBCL=sbcl
ASC2BIN=./bin/asc2bin.com

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

test: test-blc test-compiler-hosting-blc
test-all: test-blc test-ulamb test-lazyk test-compiler-hosting-blc test-compiler-hosting-ulamb test-compiler-hosting-lazyk

#================================================================
# Tests
#================================================================
# SBCL comparison test - compare LambdaLisp outputs with Common Lisp outputs for examples/*.cl
.PRECIOUS: out/%.cl.sbcl-out
out/%.cl.sbcl-out: examples/%.cl
	mkdir -p ./out
	( $(SBCL) --script $<; echo ) > $@

.PRECIOUS: out/%.cl.blc-out
out/%.cl.blc-out: examples/%.cl $(target_blc) $(BLC) $(ASC2BIN)
	mkdir -p ./out
	( cat $(target_blc) | $(ASC2BIN); cat $< ) | $(BLC) > $@

.PRECIOUS: out/%.cl.ulamb-out
out/%.cl.ulamb-out: examples/%.cl $(target_ulamb) $(ULAMB) $(ASC2BIN)
	mkdir -p ./out
	( cat $(target_ulamb) | $(ASC2BIN); cat $< ) | $(ULAMB) > $@

.PRECIOUS: out/%.cl.lazyk-out
out/%.cl.lazyk-out: examples/%.cl $(target_lazy) $(LAZYK)
	mkdir -p ./out
	cat $< | $(LAZYK) $(target_lazy) -u > $@

out/%.cleaned: out/%
# Remove the initial '> ' printed by LambdaLisp's REPL when comparing with SBCL's output
	cat $< | sed -e '1s/> //' > $<.cleaned

out/%.blc-out.diff: ./out/%.blc-out.cleaned ./out/%.sbcl-out
	cmp $^ || exit 1

out/%.ulamb-out.diff: ./out/%.ulamb-out.cleaned ./out/%.sbcl-out
	cmp $^ || exit 1

out/%.lazyk-out.diff: ./out/%.lazyk-out.cleaned ./out/%.sbcl-out
	cmp $^ || exit 1

test-blc: $(addsuffix .blc-out.diff, $(addprefix out/, $(notdir $(wildcard examples/*.cl))))
	@echo "All tests have passed for BLC with the Blc interpreter."

test-ulamb: $(addsuffix .blc-out.diff, $(addprefix out/, $(notdir $(wildcard examples/*.cl))))
	@echo "All tests have passed for Universal Lambda."

test-lazyk: $(addsuffix .blc-out.diff, $(addprefix out/, $(notdir $(wildcard examples/*.cl))))
	@echo "All tests have passed for Lazy K."


# Compiler hosting test - execute the output of examples/lambdacraft.cl as a binary lambda calculus program
out/lambdacraft.cl.blc-out.cleaned.blc-out: out/lambdacraft.cl.blc-out.cleaned $(BLC) $(ASC2BIN)
	out/lambdacraft.cl.blc-out.cleaned | $(ASC2BIN) | $(BLC) > out/lambdacraft.cl.blc-out.cleaned.blc-out
	printf 'A' > out/lambdacraft.cl.blc-expected
	cmp $< out/lambdacraft.cl.blc-expected || exit 1

test-compiler-hosting-blc: out/lambdacraft.cl.blc-out.cleaned.blc-out
	@echo "LambdaCraft-compiler-hosting-on-LambdaLisp test passed."


# Self-hosting test - compile LambdaLisp's own source code written in Common Lisp using the LambdaLisp interpreter (untested)
test-self-host: $(BASE_SRCS) $(def_prelude) ./src/main.cl $(target_blc) $(BLC) $(ASC2BIN)
	mkdir -p ./test
	( echo '(defparameter **lambdalisp-suppress-repl** t)'; \
	  cat ./src/lambdacraft.cl ./src/build/def-prelude.cl ./src/lambdalisp.cl ./src/main.cl ) > ./test/lambdalisp-src-cat.cl
	( cat $(target_blc) | $(ASC2BIN); cat ./test/lambdalisp-src-cat.cl ) | $(BLC) > ./test/lambdalisp.cl.blc.repl.out
	cat ./test/lambdalisp.cl.blc.repl.out | sed -e '1s/> //' | ./test/lambdalisp.cl.blc.script.out
	cmp ./test/lambdalisp.cl.blc.script.out $(target_blc) || (echo "The LambdaLisp output does not match with the Common Lisp output." && exit 1)
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
	cd build/clamb; gcc -O2 clamb.c -o clamb
	mv build/clamb/clamb ./bin
	chmod 755 $(ULAMB)

./build/lazyk/lazyk.c:
	mkdir -p ./build
	cd build; git clone https://github.com/irori/lazyk

$(LAZYK): ./build/lazyk/lazyk.c
	mkdir -p ./bin
	cd build/lazyk; gcc -O2 lazyk.c -o lazyk
	mv build/lazyk/lazyk ./bin
	chmod 755 $(LAZYK)

show_tromp.c_message:
	@echo
	@echo "    This procedure requires the binary lambda calculus interpreter 'tromp'."
	@echo "    To compile it and proceed, please place tromp.c under ./build."
	@echo "    Please see README.md for details."
	@echo
	@exit 1

build/tromp.c:
	mkdir -p ./build
	if [ ! -f $@ ]; then $(MAKE) show_tromp.c_message; fi

$(TROMP): ./build/tromp.c
	mkdir -p ./bin
	# Compile with the option -DM=9999999 (larger than the original -DM=999999) to execute large programs
	cd build; cc -DM=9999999 -m64 -std=c99 tromp.c -o tromp
	mv build/tromp ./bin
	chmod 755 $(TROMP)


show_Blc.S_message:
	@echo
	@echo "    This procedure requires the binary lambda calculus interpreter 'Blc'."
	@echo "    To compile it and proceed, please place Blc.S and flat.lds under ./build."
	@echo "    (Please use the uppercase Blc.S, and not the lowercase blc.S.)"
	@echo "    Blc can be run on x86-64 Linux systems. For other setups, ./bin/tromp and 'make test-blc-tromp' can be used."
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
	cd build; cc -c -o Blc.o Blc.ext.S
	cd build; ld.bfd -o Blc Blc.o -T flat.lds
	mv build/Blc ./bin
	chmod 755 $(BLC)
