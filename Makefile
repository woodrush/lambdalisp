# This setting needs to be changed on a Mac to compile tromp.c (make ./bin/tromp, make test-blc-tromp, etc.).
# Please see README.md for details.
CC=cc

BLC=./bin/Blc
LAMBDA=./bin/lambda
UNI=./bin/uni
UNI2=./bin/Uni2
TROMP=./bin/tromp
ULAMB=./bin/clamb
LAZYK=./bin/lazyk
SBCL=sbcl
LATEX=latex
DVIPDFMX=dvipdfmx

ASC2BIN=./bin/asc2bin
ASC2BIT=./bin/asc2bit

target_blc=./bin/lambdalisp.blc
target_ulamb=./bin/lambdalisp.ulamb
target_lazyk=./bin/lambdalisp.lazy
target_bitblc=./bin/lambdalisp.bitblc
target_latex=out/lambdalisp.tex
target_pdf=lambdalisp.pdf

def_prelude=./src/build/def-prelude.cl
def_prelude_lazyk=./src/build/def-prelude-lazyk.cl

BASE_SRCS=./src/lambdalisp.cl ./src/lambdacraft.cl ./src/prelude.lisp


all:
	$(MAKE) $(target_blc)
	$(MAKE) $(target_ulamb)

run-repl $(target_blc): $(BLC) $(ASC2BIN)
	( cat $(target_blc) | $(ASC2BIN); cat ) | $(BLC)

run-repl-lambda: $(target_blc) $(LAMBDA) $(ASC2BIN)
	( cat $(target_blc) | $(ASC2BIN); cat ) | $(LAMBDA) -b

run-repl-ulamb: $(target_ulamb) $(ULAMB) $(ASC2BIN)
	( cat $(target_ulamb) | $(ASC2BIN); cat ) | $(ULAMB) -u

run-repl-lazyk: $(target_lazyk)$(LAZYK) $(ASC2BIN)
	$(LAZYK) -u $(target_lazyk)

run-repl-bitblc: $(target_bitblc) $(UNI2) $(ASC2BIN) $(ASC2BIT)
	( cat $(target_bitblc); (cat | $(ASC2BIT)) ) | $(UNI2) | $(ASC2BIN)

test: test-blc-uni test-compiler-hosting-blc-uni
test-all-nonlinux: interpreters-nonlinux test-blc-uni test-ulamb test-lazyk test-compiler-hosting-blc-uni test-blc-tromp test-blc-lambda
# On x86-64-Linux, the interpreter 'Blc' can be used.
test-all: interpreters test-blc-uni test-ulamb test-lazyk test-compiler-hosting-blc-uni test-blc-tromp test-blc-lambda test-blc test-compiler-hosting-blc

# Build all of the interpreters that support LambdaLisp
interpreters: uni clamb lazyk tromp blc lambda asc2bin
interpreters-nonlinux: uni clamb lazyk tromp lambda asc2bin

# Build the PDF file
pdf: $(target_pdf)


#================================================================
# Tests
#================================================================
# Each basic test compares LambdaLisp outputs with:
# - Outputs when executed on Common Lisp, for examples/*.cl, which run on both Common Lisp and LambdaLisp
# - Predefined expected output text in ./test/, for examples/*.lisp, which are LambdaLisp-exclusive programs
.PHONY: test-lisp-%
test-lisp-%: $(addsuffix .%-out.expected-diff, $(addprefix out/, $(notdir $(wildcard test/*.lisp.out))))
	@echo "\n    All LambdaLisp-exclusive tests have passed for $(interpreter-name-$*).\n"

.PHONY: test-sbclcmp-%
test-sbclcmp-%: $(addsuffix .%-out.sbcl-diff, $(addprefix out/, $(notdir $(wildcard examples/*.cl))))
	@echo "\n    All SBCL comparison tests have passed for $(interpreter-name-$*).\n"

.PHONY: test-%
test-%: test-sbclcmp-% test-lisp-%
	@echo "\n    All tests have passed for $(interpreter-name-$*).\n"
interpreter-name-blc="BLC with the interpreter 'Blc'"
interpreter-name-blc-lambda="BLC with the interpreter 'lambda'"
interpreter-name-blc-tromp="BLC with the interpreter 'tromp'"
interpreter-name-blc-uni="BLC with the interpreter 'uni'"
interpreter-name-ulamb="Universal Lambda"
interpreter-name-lazyk="Lazy K"


# Compiler hosting test - execute the output of examples/lambdacraft.cl as a binary lambda calculus program
.PHONY: test-compiler-hosting-blc
test-compiler-hosting-blc: out/lambdacraft.cl.blc-out $(BLC) $(ASC2BIN) examples/lambdacraft.cl
# Remove non-01-characters and provide it to BLC
	cat $< | sed 's/[^0-9]*//g' | tr -d "\n" | $(ASC2BIN) | $(BLC) > out/$@.out
	printf 'A' > out/lambdacraft.cl.blc-expected
	diff out/$@.out out/lambdacraft.cl.blc-expected || ( rm out/$@.out; exit 1)
	@echo "\n    LambdaCraft-compiler-hosting-on-LambdaLisp test passed.\n"

.PHONY: test-compiler-hosting-blc-uni
test-compiler-hosting-blc-uni: out/lambdacraft.cl.blc-uni-out $(UNI) $(ASC2BIN) examples/lambdacraft.cl
# Remove non-01-characters and provide it to BLC
	cat $< | sed 's/[^0-9]*//g' | tr -d "\n" | $(ASC2BIN) | $(UNI) > out/$@.out
	printf 'A' > out/lambdacraft.cl.blc-expected
	diff out/$@.out out/lambdacraft.cl.blc-expected || ( rm out/$@.out; exit 1)
	@echo "\n    LambdaCraft-compiler-hosting-on-LambdaLisp test passed.\n"


# Self-hosting test - compile LambdaLisp's own source code written in Common Lisp using LambdaLisp (currently theoretical - requires a lot of time and memory)
.PHONY: test-self-host
test-self-host: $(BASE_SRCS) $(def_prelude) ./src/main.cl $(target_blc) $(BLC) $(ASC2BIN)
	mkdir -p ./test
	( echo '(defparameter **lambdalisp-suppress-repl** t)'; \
	  cat ./src/lambdacraft.cl ./src/build/def-prelude.cl ./src/lambdalisp.cl ./src/main.cl ) > ./out/lambdalisp-src-cat.cl
	( cat $(target_blc) | $(ASC2BIN); cat ./out/lambdalisp-src-cat.cl ) | $(BLC) > ./out/lambdalisp.cl.blc.repl.out
	cat ./out/lambdalisp.cl.blc.repl.out | sed -e '1s/> //' | ./out/lambdalisp.cl.blc.script.out
	diff ./out/lambdalisp.cl.blc.script.out $(target_blc) || (echo "The LambdaLisp output does not match with the Common Lisp output." && exit 1)
	@echo "\n    LambdaLisp self-hosting test passed.\n"


# How to execute the programs in each platform
.PRECIOUS: out/%.cl.sbcl-out
out/%.cl.sbcl-out: examples/%.cl
	mkdir -p ./out
	if [ -f "test/$*.cl.in" ]; then \
		cat test/$*.cl.in | $(SBCL) --script $< > $@.tmp; else \
		$(SBCL) --script $< > $@.tmp; fi
	mv $@.tmp $@

.PRECIOUS: out/%.blc-out
out/%.blc-out: examples/% $(target_blc) $(BLC) $(ASC2BIN)
	mkdir -p ./out
	if [ -f "test/$*.in" ]; then \
		( cat $(target_blc) | $(ASC2BIN); cat $< test/$*.in ) | $(BLC) > $@.tmp; else \
		( cat $(target_blc) | $(ASC2BIN); cat $< ) | $(BLC) > $@.tmp; fi
	mv $@.tmp $@

.PRECIOUS: out/%.blc-lambda-out
out/%.blc-lambda-out: examples/% $(target_blc) $(LAMBDA) $(ASC2BIN)
	mkdir -p ./out
	if [ -f "test/$*.in" ]; then \
		( cat $(target_blc) | $(ASC2BIN); cat $< test/$*.in ) | $(LAMBDA) -b > $@.tmp; else \
		( cat $(target_blc) | $(ASC2BIN); cat $< ) | $(LAMBDA) -b > $@.tmp; fi
	mv $@.tmp $@

.PRECIOUS: out/%.blc-tromp-out
out/%.blc-tromp-out: examples/% $(target_blc) $(TROMP) $(ASC2BIN)
	mkdir -p ./out
	if [ -f "test/$*.in" ]; then \
		( cat $(target_blc) | $(ASC2BIN); cat $< test/$*.in) | $(TROMP) > $@.tmp; else \
		( cat $(target_blc) | $(ASC2BIN); cat $< ) | $(TROMP) > $@.tmp; fi
	mv $@.tmp $@

.PRECIOUS: out/%.blc-uni-out
out/%.blc-uni-out: examples/% $(target_blc) $(UNI) $(ASC2BIN)
	mkdir -p ./out
	if [ -f "test/$*.in" ]; then \
		( cat $(target_blc) | $(ASC2BIN); cat $< test/$*.in ) | $(UNI) > $@.tmp; else \
		( cat $(target_blc) | $(ASC2BIN); cat $< ) | $(UNI) > $@.tmp; fi
	mv $@.tmp $@

.PRECIOUS: out/%.ulamb-out
out/%.ulamb-out: examples/% $(target_ulamb) $(ULAMB) $(ASC2BIN)
	mkdir -p ./out
	if [ -f "test/$*.in" ]; then \
		( cat $(target_ulamb) | $(ASC2BIN); cat $< test/$*.in ) | $(ULAMB) -u > $@.tmp; else \
		( cat $(target_ulamb) | $(ASC2BIN); cat $< ) | $(ULAMB) -u > $@.tmp; fi
	mv $@.tmp $@

.PRECIOUS: out/%.lazyk-out
out/%.lazyk-out: examples/% $(target_lazyk) $(LAZYK)
	mkdir -p ./out
	if [ -f "test/$*.in" ]; then \
		cat $< $*.in | $(LAZYK) $(target_lazyk) -u > $@.tmp; else \
		cat $< | $(LAZYK) $(target_lazyk) -u > $@.tmp; fi
	mv $@.tmp $@


# SBCL comparison test - compare LambdaLisp outputs with Common Lisp outputs for examples/*.cl
out/%.blc-out.sbcl-diff: ./out/%.blc-out.cleaned ./out/%.sbcl-out
	diff $^ || exit 1

out/%.blc-lambda-out.sbcl-diff: ./out/%.blc-lambda-out.cleaned ./out/%.sbcl-out
	diff $^ || exit 1

out/%.blc-tromp-out.sbcl-diff: ./out/%.blc-tromp-out.cleaned ./out/%.sbcl-out
	diff $^ || exit 1

out/%.blc-uni-out.sbcl-diff: ./out/%.blc-uni-out.cleaned ./out/%.sbcl-out
	diff $^ || exit 1

out/%.ulamb-out.sbcl-diff: ./out/%.ulamb-out.cleaned ./out/%.sbcl-out
	diff $^ || exit 1

out/%.lazyk-out.sbcl-diff: ./out/%.lazyk-out.cleaned ./out/%.sbcl-out
	diff $^ || exit 1

# Remove the initial '> ' printed by LambdaLisp's REPL when comparing with SBCL's output
out/%.cleaned: out/%
	cat $< | sed -e '1s/> //' > $<.cleaned


# Expected text comparison test - compare LambdaLisp outputs with a predefined expected output
out/%.blc-out.expected-diff: ./out/%.blc-out ./test/%.out
	diff $^ || exit 1

out/%.blc-lambda-out.expected-diff: ./out/%.blc-lambda-out ./test/%.out
	diff $^ || exit 1

out/%.blc-tromp-out.expected-diff: ./out/%.blc-tromp-out ./test/%.out
	diff $^ || exit 1

out/%.blc-uni-out.expected-diff: ./out/%.blc-uni-out ./test/%.out
	diff $^ || exit 1

out/%.ulamb-out.expected-diff: ./out/%.ulamb-out ./test/%.out
	diff $^ || exit 1

out/%.lazyk-out.expected-diff: ./out/%.lazyk-out ./test/%.out
	diff $^ || exit 1


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
.PHONY: blc-src
blc-src: $(target_blc)
$(target_blc): $(BASE_SRCS) $(def_prelude) ./src/main.cl
	cd src; sbcl --script main.cl > ../$(target_blc).tmp
	mv $(target_blc).tmp $(target_blc)

.PHONY: ulamb-src
ulamb-src: $(target_ulamb)
$(target_ulamb): $(BASE_SRCS) $(def_prelude) ./src/main-ulamb.cl ./src/lazyk-ulamb-blc-wrapper.cl
	cd src; sbcl --script ./main-ulamb.cl > ../$(target_ulamb).tmp
	mv $(target_ulamb).tmp $(target_ulamb)

.PHONY: lazyk-src
lazyk-src: $(target_lazyk)
$(target_lazyk): $(BASE_SRCS) $(def_prelude_lazyk) ./src/main-lazyk.cl ./src/lazyk-ulamb-blc-wrapper.cl ./src/lazyk-chars.cl
	@echo "Compiling to Lazy K takes a while (several minutes)."
	cd src; sbcl --script ./main-lazyk.cl > ../$(target_lazyk).tmp

	# Replace ``s`kki with k, which are equivalent terms
	cat $(target_lazyk).tmp | sed s/\`\`s\`kki/k/g > $(target_lazyk).tmp2
	cat $(target_lazyk).tmp2 | sed -e 's/\(................................................................................\)/\1\n/g' > $(target_lazyk).tmp
	mv $(target_lazyk).tmp $(target_lazyk)
	rm $(target_lazyk).tmp2

.PHONY: bitblc-src
bitblc-src: $(target_bitblc)
$(target_bitblc): $(BASE_SRCS) $(def_prelude) ./src/main-bitblc.cl ./src/lazyk-ulamb-blc-wrapper.cl
	cd src; sbcl --script ./main-bitblc.cl > ../$@.tmp
	mv $@.tmp $@


# Additional targets
.PRECIOUS: $(target_latex)
$(target_latex): $(BASE_SRCS) $(def_prelude) ./src/main-latex.cl ./tools/main.tex ./tools/make-latex.sh
	mkdir -p ./out
	./tools/make-latex.sh
	mv lambdalisp.tex out

.PHONY: pdf
$(target_pdf): $(target_latex)
	cp ./tools/main.tex out
	cd out; $(LATEX) main.tex
	cd out; $(DVIPDFMX) main.dvi -o lambdalisp.pdf
	mv out/lambdalisp.pdf .


#================================================================
# Building the interpreters
#================================================================
./build/clamb/clamb.c:
	mkdir -p ./build
	cd build; git clone https://github.com/irori/clamb

$(ULAMB): ./build/clamb/clamb.c
	cd build/clamb; $(CC) -O2 clamb.c -o clamb
	mv build/clamb/clamb ./bin
	chmod 755 $(ULAMB)

.PHONY: clamb
clamb: $(ULAMB)

./build/lazyk/lazyk.c:
	mkdir -p ./build
	cd build; git clone https://github.com/irori/lazyk

$(LAZYK): ./build/lazyk/lazyk.c
	cd build/lazyk; $(CC) -O2 lazyk.c -o lazyk
	mv build/lazyk/lazyk ./bin
	chmod 755 $(LAZYK)

.PHONY: lazyk
lazyk: $(LAZYK)

build/Blc.S:
	mkdir -p ./build
	wget https://justine.lol/lambda/Blc.S?v=2
	mv Blc.S?v=2 ./build/Blc.S

build/flat.lds:
	mkdir -p ./build
	wget https://justine.lol/lambda/flat.lds
	mv flat.lds ./build

$(BLC): build/Blc.S build/flat.lds
	# Extend the maximum memory limit to execute large programs
	# Make TERMS configurable
	cd build; cat Blc.S | sed -e 's/#define.*TERMS.*//' > Blc.ext.S
	# Compile with the option -DTERMS=50000000 (larger than the original -DTERMS=5000000) to execute large programs
	cd build; $(CC) -c -DTERMS=50000000 -o Blc.o Blc.ext.S
	cd build; ld.bfd -o Blc Blc.o -T flat.lds
	mv build/Blc ./bin
	chmod 755 $(BLC)

.PHONY: blc
blc: $(BLC)

build/lambda/blc.orig.h:
	mkdir -p ./build/lambda
	wget https://justine.lol/lambda/blc.h
	mv blc.h ./build/lambda/blc.orig.h

build/lambda/lambda.c:
	mkdir -p ./build/lambda
	wget https://justine.lol/lambda/lambda.c
	mv lambda.c ./build/lambda

build/lambda/parse.c:
	mkdir -p ./build/lambda
	wget https://justine.lol/lambda/parse.c
	mv parse.c ./build/lambda

build/lambda/needbit.c:
	mkdir -p ./build/lambda
	wget https://justine.lol/lambda/needbit.c
	mv needbit.c ./build/lambda

build/lambda/getbit.c:
	mkdir -p ./build/lambda
	wget https://justine.lol/lambda/getbit.c
	mv getbit.c ./build/lambda

build/lambda/error.c:
	mkdir -p ./build/lambda
	wget https://justine.lol/lambda/error.c
	mv error.c ./build/lambda

build/lambda/debug.c:
	mkdir -p ./build/lambda
	wget https://justine.lol/lambda/debug.c
	mv debug.c ./build/lambda

build/lambda/dump.c:
	mkdir -p ./build/lambda
	wget https://justine.lol/lambda/dump.c
	mv dump.c ./build/lambda

build/lambda/print.c:
	mkdir -p ./build/lambda
	wget https://justine.lol/lambda/print.c
	mv print.c ./build/lambda

build/lambda/vars.c:
	mkdir -p ./build/lambda
	wget https://justine.lol/lambda/vars.c
	mv vars.c ./build/lambda

$(LAMBDA):	build/lambda/blc.orig.h build/lambda/lambda.c build/lambda/parse.c \
			build/lambda/needbit.c build/lambda/getbit.c build/lambda/error.c \
			build/lambda/debug.c build/lambda/dump.c build/lambda/print.c \
			build/lambda/vars.c
	# Extend the maximum memory limit to execute large programs
	# Make TERMS configurable
	cd build/lambda; cat blc.orig.h | sed -e 's/#define.*TERMS.*//' > blc.h
	# Compile with the option -DTERMS=50000000 (larger than the original -DTERMS=5000000) to execute large programs
	cd build/lambda; $(CC) -I . -DTERMS=50000000 -o lambda lambda.c \
		parse.c needbit.c getbit.c error.c debug.c dump.c print.c vars.c
	mv build/lambda/lambda ./bin

.PHONY: lambda
lambda: $(LAMBDA)

build/tromp.c:
	mkdir -p ./build
	wget http://www.ioccc.org/2012/tromp/tromp.c
	mv tromp.c ./build

$(TROMP): ./build/tromp.c
	# Compile with the option -DA=9999999 (larger than the original -DM=999999) to execute large programs
	cd build; $(CC) -Wall -W -std=c99 -O2 -m64 -DInt=long -DA=9999999 -DX=8 tromp.c -o tromp
	mv build/tromp ./bin
	chmod 755 $(TROMP)

.PHONY: tromp
tromp: $(TROMP)

build/uni.c:
	mkdir -p ./build
	wget https://tromp.github.io/cl/uni.c
	mv uni.c ./build

$(UNI): ./build/uni.c
	# Compile with the option -DA=9999999 (larger than the original -DM=999999) to execute large programs
	cd build; $(CC) -Wall -W -O2 -std=c99 -m64 -DM=9999999 uni.c -o uni
	mv build/uni ./bin
	chmod 755 $(UNI)

.PHONY: uni
uni: $(UNI)

.PHONY: asc2bin
asc2bin: $(ASC2BIN)
$(ASC2BIN): ./tools/asc2bin.c
	cd build; $(CC) ../tools/asc2bin.c -O2 -o asc2bin
	mv build/asc2bin ./bin
	chmod 755 $(ASC2BIN)

.PHONY: asc2bit
asc2bit: $(ASC2BIT)
$(ASC2BIT): tools/asc2bit.c
	$(CC) $< -O2 -o $@
	chmod 755 $@
