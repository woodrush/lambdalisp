BLC=./bin/Blc
TROMP=./bin/tromp
ULAMB=./bin/clamb/clamb
LAZYK=./bin/lazyk/lazyk
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


#================================================================
# Tests
#================================================================
# SBCL comparison test - compare LambdaLisp outputs with Common Lisp outputs for examples/*.cl
test/%.cl.blc.out : examples/%.cl $(target_blc) $(BLC) $(ASC2BIN)
	mkdir -p ./test
	( cat $(target_blc) | $(ASC2BIN); cat $< ) | $(BLC) | sed -e '1s/> //' > ./test/$(notdir $<).blc.out
	( $(SBCL) --script $<; echo ) > ./test/$(notdir $<).sbcl.out
	cmp ./test/$(notdir $<).blc.out ./test/$(notdir $<).sbcl.out || (rm ./test/$(notdir $<).blc.out && echo "Test failed at $<" && exit 1)

test-blc : $(addprefix test/, $(addsuffix .blc.out, $(notdir $(wildcard examples/*.cl))))
	@echo "All tests have passed for BLC."

test/%.cl.ulamb.out : examples/%.cl $(target_ulamb) $(ULAMB) $(ASC2BIN)
	mkdir -p ./test
	( cat $(target_ulamb) | $(ASC2BIN); cat $< ) | $(ULAMB) | sed -e '1s/> //' > ./test/$(notdir $<).ulamb.out
	( $(SBCL) --script $<; echo ) > ./test/$(notdir $<).sbcl.out
	cmp ./test/$(notdir $<).ulamb.out ./test/$(notdir $<).sbcl.out || (echo "Test failed at $<" && exit 1)

test-ulamb : $(addprefix test/, $(addsuffix .ulamb.out, $(notdir $(wildcard examples/*.cl))))
	@echo "All tests have passed for Universal Lambda."

test/%.cl.lazy.out : examples/%.cl $(target_lazy) $(LAZYK)
	mkdir -p ./test
	cat $< | $(LAZYK) $(target_lazy) -u | sed -e '1s/> //' > ./test/$(notdir $<).lazy.out
	( $(SBCL) --script $<; echo ) > ./test/$(notdir $<).sbcl.out
	cmp ./test/$(notdir $<).lazy.out ./test/$(notdir $<).sbcl.out || (echo "Test failed at $<" && exit 1)

test-lazyk : $(addprefix test/, $(addsuffix .lazy.out, $(notdir $(wildcard examples/*.cl))))
	@echo "All tests have passed for Lazy K."


# Compiler hosting test - execute the output of examples/lambdacraft.cl as a binary lambda calculus program
test/lambdacraft.cl.blc.out.blc.out: test/lambdacraft.cl.blc.out $(BLC) $(ASC2BIN)
	cat test/lambdacraft.cl.blc.out | sed 's/[^0-9]*//g' | tr -d "\n" | $(ASC2BIN) | $(BLC) > test/lambdacraft.cl.blc.out.blc.out
	printf 'A' > test/lambdacraft.cl.blc.out.blc.out.expected
	cmp test/lambdacraft.cl.blc.out.blc.out test/lambdacraft.cl.blc.out.blc.out.expected || (echo "Output does not match with 'A'" && exit 1)

test-compiler-hosting-blc: test/lambdacraft.cl.blc.out.blc.out
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
./bin/clamb/clamb.c:
	mkdir -p ./bin
	cd bin; git clone https://github.com/irori/clamb

$(ULAMB): ./bin/clamb/clamb.c
	cd bin/clamb; gcc -O2 clamb.c -o clamb
	chmod 755 $(ULAMB)

./bin/lazyk/lazyk.c:
	mkdir -p ./bin
	cd bin; git clone https://github.com/irori/lazyk

$(LAZYK): ./bin/lazyk/lazyk.c
	cd bin/lazyk; gcc -O2 lazyk.c -o lazyk
	chmod 755 $(LAZYK)

bin/tromp.c:
	mkdir -p ./bin
	@echo "    This procedure requires the binary lambda calculus interpreter 'tromp'."
	@echo "    To compile it and proceed, please place tromp.c under ./bin."
	@echo "    Please see README.md for details."
	@exit 1

$(TROMP): ./bin/tromp.c
	# Compile with the option -DM=9999999 (larger than the original -DM=999999) to execute large programs
	cd bin; cc -DM=9999999 -m64 -std=c99 tromp.c -o tromp
	chmod 755 $(TROMP)


show_blc.s_message:
	mkdir -p ./bin
	@echo "    This procedure requires the binary lambda calculus interpreter 'Blc'."
	@echo "    To compile it and proceed, please place Blc.s and flat.lds under ./bin."
	@echo "    Blc can be run on x86-64 Linux systems. For other setups, ./bin/tromp and 'make test-blc-tromp' can be used."
	@echo "    Please see README.md for details."
	@exit 1

./bin/Blc.s: show_blc.s_message
./bin/flat.lds: show_blc.s_message


$(BLC): ./bin/Blc.s ./bin/flat.lds
	# Extend the maximum memory limit to execute large programs
	cd bin; cat Blc.S | sed -e 's/#define TERMS	5000000/#define TERMS	50000000/' > Blc.ext.S
	cd bin; cc -c -o Blc.o Blc.ext.S
	cd bin; ld.bfd -o Blc Blc.o -T flat.lds
	chmod 755 $(BLC)
