target_blc=lambdalisp.blc

lambdalisp_cl=./lambdalisp.cl
lambdacraft_cl=./lazy.cl
def_prelude=./src/def-prelude.cl
prelude_lisp=./src/prelude.lisp

all:
	$(MAKE) $(target_blc)

$(target_blc): $(def_prelude) $(lambdalisp_cl) $(lambdacraft_cl)
	sbcl --script $(lambdalisp_cl) > $(target_blc)

$(def_prelude): $(prelude_lisp)
	./compile-prelude.sh > $(def_prelude)
