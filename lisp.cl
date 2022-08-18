(load "./lazy.cl")


(def-lazy stringtermchar 256)
(def-lazy stringterm (inflist 256))

(def-lazy "(" (+ 32 8))
(def-lazy ")" (succ "("))
(def-lazy " " 32)
(def-lazy "." (+ (+ (+ 32 8) 4) 2))
(def-lazy ">" (+ (+ (+ (+ 32 16) 8) 4) 2))
(def-lazy "*" (+ (+ 32 8) 2))
(def-lazy "\\n" (+ 8 2))

(def-lazy 3 (succ 2))
(def-lazy 5 (succ 4))
(def-lazy 6 (+ 4 2))
(def-lazy 7 (succ 6))
(def-lazy 9 (succ 8))
(def-lazy 10 (succ 9))
(def-lazy 11 (succ 10))
(def-lazy 14 (+ 8 (+ 4 2)))
(def-lazy 15 (succ (+ 8 (+ 4 2))))


(defrec-lazy map (f list)
  (if (isnil list)
    nil
    (cons (f (car list)) (map f (cdr list)))))

(defrec-lazy length (list)
  ((letrec-lazy length (l n)
      (if (isnil l)
        n
        (length (cdr l) (succ n))))
    list 0))

(defmacro-lazy await-list (list body)
  `(if (<= 0 (length ,list))
    ,body
    ,body))

(defrec-lazy append-element (l item)
  (if (isnil l) (cons item nil) (cons (car l) (append-element (cdr l) item))))

(defrec-lazy append-list (l item)
  (if (isnil l) item (cons (car l) (append-list (cdr l) item))))

(defun-lazy truth-data (expr)
  (if expr t-data nil))

(defun-lazy type-atom (t0 t1) t0)
(defun-lazy type-list (t0 t1) t1)

(def-lazy typeof car)
(def-lazy valueof cdr)

(defmacro-lazy typematch (expr atomcase listcase nilcase)
  `(if (isnil ,expr)
      ,nilcase
      ((typeof ,expr) ,atomcase ,listcase)))

(defun-lazy isatom (expr)
  (typematch expr
    ;; atom
    t
    ;; list
    nil
    ;; nil
    t))

(defmacro-lazy car-data (data)
  `(car (valueof ,data)))

(defmacro-lazy cdr-data (data)
  `(cdr (valueof ,data)))

(defmacro-lazy cons-data (x y)
  `(cons type-list (cons ,x ,y)))

(defmacro-lazy atom* (value)
  `(cons type-atom ,value))

(defmacro-lazy list* (arg &rest args)
  (if (not args)
    `(cons-data ,arg nil)
    `(cons-data ,arg (list* ,@args))))

(defrec-lazy append-element-data (l item)
  (if (isnil l) (cons-data item nil) (cons-data (car-data l) (append-element-data (cdr-data l) item))))

(defrec-lazy map-data-as-baselist (f data)
  (cond ((isnil data) nil)
        (t (cons (f (car-data data))
                 (map-data-as-baselist f (cdr-data data))))))

(defun-lazy printatom (atomenv expr cont)
  (append-list (car ((valueof expr) cdr atomenv)) cont))

(defrec-lazy printexpr-helper (atomenv expr mode cont)
  (if mode
    (typematch expr
      ;; atom
      (printatom atomenv expr cont)
      ;; list
      (cons "(" (printexpr-helper atomenv expr nil (cons ")" cont)))
      ;; nil
      (cons "(" (cons ")" cont)))
    (printexpr-helper atomenv (car-data expr) t
      (typematch (cdr-data expr)
        ;; atom
        (cons " " (cons "." (cons " " (printatom atomenv (cdr-data expr) cont))))
        ;; list
        (cons " " (printexpr-helper atomenv (cdr-data expr) nil cont))
        ;; nil
        cont))))

(defun-lazy printexpr (atomenv expr cont)
  (printexpr-helper atomenv expr t cont))

(defrec-lazy reverse (list curlist)
  (if (isnil list) curlist (reverse (cdr list) (cons (car list) curlist))))

(defrec-lazy read-string (curstr stdin)
  (let ((c (car stdin)))
    (cond
          ((or (= "(" c) (= ")" c) (= " " c) (= "\\n" c) (= stringtermchar c))
            (cons (reverse curstr nil) stdin))
          (t
            (read-string (cons (car stdin) curstr) (cdr stdin))))))

(defun-lazy get-atomindex-env (atomenv str)
  ((letrec-lazy get-atomindex-env-helper (cur-atomenv n)
    (cond ((isnil cur-atomenv)
            (cons n (append-element atomenv str)))
          ((stringeq (car cur-atomenv) str)
            (cons n atomenv))
          (t
            (get-atomindex-env-helper (cdr cur-atomenv) (succ n))))
            )
   atomenv 0))

(defun-lazy read-atom (stdin atomenv)
  (let ((ret (read-string nil stdin))
        (retstr (car ret))
        (stdin (cdr ret))
        (ret-atomlookup (get-atomindex-env atomenv retstr))
        (ret-atom (atom* (car ret-atomlookup)))
        (atomenv (cdr ret-atomlookup)))
    (cons ret-atom (cons stdin atomenv))))

(defrec-lazy stringeq (s1 s2)
  (cond ((and (isnil s1) (isnil s2))
          t)
        ((or  (and (not (isnil s1)) (isnil s2))
              (and (isnil s1) (not (isnil s2))))
          nil)
        ((= (car s1) (car s2))
          (stringeq (cdr s1) (cdr s2)))
        (t
          nil)))

(defrec-lazy read-skip-whitespace (stdin)
  (let ((c (car stdin)))
    (cond ((or (= " " c) (= "\\n" c) (= stringtermchar c))
            (read-skip-whitespace (cdr stdin)))
          (t
            stdin))))

(defrec-lazy reverse-base2data (list curlist)
  (if (isnil list) curlist (reverse-base2data (cdr list) (cons-data (car list) curlist))))

(defrec-lazy read-expr (stdin atomenv)
  (let ((read-list
          (letrec-lazy read-list (stdin atomenv curexpr)
            (let ((stdin (read-skip-whitespace stdin))
                  (c (car stdin)))
              (cond ((= ")" c)
                      (cons (reverse-base2data curexpr nil) (cons (cdr stdin) atomenv)))
                    (t
                      (let ((ret (read-expr stdin atomenv))
                            (ret-expr (car ret))
                            (stdin (car (cdr ret)))
                            (atomenv (cdr (cdr ret))))
                        (read-list stdin atomenv (cons ret-expr curexpr)))))))))
    (let ((stdin (read-skip-whitespace stdin)) (c (car stdin)))
            (cond ((= "(" c)
                    (read-list (cdr stdin) atomenv nil))
                  (t
                    (read-atom stdin atomenv))))))

(def-lazy initial-atomenv
  (list
    (list "q" "u" "o" "t" "e")
    (list "c" "a" "r")
    (list "c" "d" "r")
    (list "c" "o" "n" "s")
    (list "a" "t" "o" "m")
    (list "e" "q")
    (list "c" "o" "n" "d")
    (list "p" "r" "i" "n" "t")
    (list "r" "e" "a" "d")
    (list "d" "e" "f")
    (list "l" "o" "c")
    (list "p" "r" "o" "g" "n")
    (list "w" "h" "i" "l" "e")
    (list "l" "a" "m" "b" "d" "a")
    (list "m" "a" "c" "r" "o")
    (list "t")))

(def-lazy maxforms 15)

(def-lazy initial-varenv
  (list (cons maxforms (atom* maxforms))))

(def-lazy t-data
  (atom* maxforms))

(defrec-lazy eval-cond (clauselist evalret cont)
  (let ((carclause (-> clauselist car-data))
        (carcond (-> carclause car-data))
        (carbody (-> carclause cdr-data)))
    (eval carcond evalret
      (lambda (expr evalret)
        (if (isnil expr)
          (eval-cond (cdr-data clauselist) evalret cont)
          (eval-progn carbody evalret cont))))))

(defrec-lazy varenv-lookup (varenv varval)
  (let ((pair (car varenv))
        (evarval (car pair))
        (ebody (cdr pair)))
    (cond ((isnil varenv)
            nil)
          ((= varval evarval)
            ebody)
          (t
            (varenv-lookup (cdr varenv) varval)))))

(defrec-lazy eval-map-base (lexpr curexpr evalret cont)
  (cond ((isnil lexpr)
          (cont curexpr evalret))
        (t
          (eval (car-data lexpr) evalret
            (lambda (expr evalret)
              (eval-map-base (cdr-data lexpr) (append-element curexpr expr) evalret cont))))))

(defrec-lazy prepend-envzip-basedata (argnames evargs env)
  (cond ((isnil argnames)
          env)
        (t
          (cons (cons (valueof (car-data argnames)) (if (isnil evargs) nil (car evargs)))
                (prepend-envzip-basedata (cdr-data argnames) (if (isnil evargs) nil (cdr evargs)) env)))))

(defun-lazy eval-lambda (lambdaexpr callargs evalret cont)
  (let ((argnames (-> lambdaexpr cdr-data car-data))
        (lambdabody (-> lambdaexpr cdr-data cdr-data))
        (varenv-orig (car evalret)))
    (eval-map-base callargs nil evalret
      (lambda (argvalues evalret)
        (let-parse-evalret* evalret varenv atomenv stdin globalenv
          (eval-progn
            lambdabody
            (evalret* (prepend-envzip-basedata argnames argvalues varenv) atomenv stdin globalenv)
            (lambda (expr evalret)
              (let-parse-evalret* evalret varenv atomenv stdin globalenv
                ;; Reset the variable environment to the original one
                (cont expr (evalret* varenv-orig atomenv stdin globalenv))))))))))

(defrec-lazy prepend-envzip-data (argnames evargs env)
  (cond ((isnil argnames)
          env)
        (t
          (cons (cons (valueof (car-data argnames)) (if (isnil evargs) nil (car-data evargs)))
                (prepend-envzip-data (cdr-data argnames) (if (isnil evargs) nil (cdr-data evargs)) env)))))

(defun-lazy eval-macro (lambdaexpr callargs evalret cont)
  (let ((argnames (-> lambdaexpr cdr-data car-data))
        (lambdabody (-> lambdaexpr cdr-data cdr-data))
        (varenv-orig (car evalret)))
    (let-parse-evalret* evalret varenv atomenv stdin globalenv
      (eval-progn
        lambdabody
        (evalret* (prepend-envzip-data argnames callargs varenv) atomenv stdin globalenv)
        (lambda (expr evalret)
          (let-parse-evalret* evalret varenv atomenv stdin globalenv
            ;; Reset the variable stack to the original one
            ;; Evaluate the constructed expression again
            (eval expr (evalret* varenv-orig atomenv stdin globalenv) cont)))))))

(defmacro-lazy evalret* (varenv atomenv stdin globalenv)
  `(list ,varenv ,atomenv ,stdin ,globalenv))

(defmacro-lazy let-parse-evalret* (evalret varenv atomenv stdin globalenv body)
  `(let ((,varenv      (-> ,evalret car))
         (,atomenv     (-> ,evalret cdr car))
         (,stdin       (-> ,evalret cdr cdr car))
         (,globalenv   (-> ,evalret cdr cdr cdr car)))
      ,body))

(defrec-lazy eval-progn (proglist evalret cont)
  (cond ((isnil proglist)
          (cont nil evalret))
        ((isnil (cdr-data proglist))
          (eval (car-data proglist) evalret cont))
        (t
          (eval (car-data proglist) evalret
            (lambda (expr evalret)
              (eval-progn (cdr-data proglist) evalret cont))))))

(defrec-lazy eval-while (condition body evalret cont)
  (eval condition evalret
    (lambda (expr evalret)
      (cond ((isnil expr)
              (cont nil evalret))
            (t
              (eval-progn body evalret
                (lambda (expr evalret)
                  (eval-while condition body evalret cont))))))))

(defrec-lazy eval-lambdalike (lambdaexpr callargs evalret cont)
  (eval lambdaexpr evalret
    (lambda (expr evalret)
      (cond
        ;; Macros - evaluate the result again
        ((= 14 (valueof (car-data expr)))
          (eval-macro expr callargs evalret cont))
        ;; Lambdas
        (t
          (eval-lambda expr callargs evalret cont))))))

(defrec-lazy eval (expr evalret cont)
  (typematch expr
    ;; atom
    (let-parse-evalret* evalret varenv atomenv stdin globalenv
      (cont (varenv-lookup
              (if (isnil globalenv)
                varenv
                (append-list varenv globalenv))
              (valueof expr))
            evalret))
    ;; list
    (let ((head (car-data expr))
          (tail (cdr-data expr))
          (head-index (valueof head)))
      (typematch head
        ;; atom
        (cond
          ;; Evaluate as a lambda
          ((<= maxforms head-index)
            (eval-lambdalike head tail evalret cont))
          ;; Evaluate as a special form
          (t
            (nth head-index
              (list
                ;; quote
                (cont (car-data tail) evalret)
                ;; car
                (eval (car-data tail) evalret
                  (lambda (expr evalret)
                    (cont (car-data expr) evalret)))
                ;; cdr
                (eval (car-data tail) evalret
                  (lambda (expr evalret)
                    (cont (cdr-data expr) evalret)))
                ;; cons
                (eval (car-data tail) evalret
                  (lambda (cons-x evalret)
                    (eval (-> tail cdr-data car-data) evalret
                      (lambda (cons-y evalret)
                        (cont (cons-data cons-x cons-y) evalret)))))
                ;; atom
                (eval (car-data tail) evalret
                  (lambda (expr evalret)
                    (cont (truth-data (isatom expr)) evalret)))
                ;; eq
                (eval (car-data tail) evalret
                  (lambda (eq-x evalret)
                    (eval (-> tail cdr-data car-data) evalret
                      (lambda (eq-y evalret)
                        (cont
                          (cond ((and (isnil eq-x) (isnil eq-y))
                                  t-data)
                                ((or (isnil eq-x) (isnil eq-y))
                                  nil)
                                ((or (not (isatom eq-x)) (not (isatom eq-y)))
                                  nil)
                                (t
                                  (truth-data (= (valueof eq-x) (valueof eq-y)))))
                          evalret)))))
                ;; cond
                (eval-cond tail evalret cont)
                ;; print
                (if (isnil tail)
                  (cons "\\n" (cont nil evalret))
                  (eval (car-data tail) evalret
                    (lambda (expr evalret)
                      (let ((atomenv (-> evalret cdr car))
                            (outstr (printexpr atomenv expr nil)))
                        ;; Control flow
                        (await-list outstr
                          (append-list outstr (cont expr evalret)))))))
                ;; read
                (let-parse-evalret* evalret varenv-old atomenv stdin globalenv
                  (let ((ret-parse (read-expr stdin atomenv))
                        (expr (-> ret-parse car))
                        (stdin (-> ret-parse cdr car))
                        (atomenv (-> ret-parse cdr cdr))
                        (varenv (car evalret)))
                      (cont expr (evalret* varenv atomenv stdin globalenv))))
                ;; def
                (let ((varname (-> tail car-data))
                      (defbody (-> tail cdr-data car-data)))
                  (eval defbody evalret
                    (lambda (expr evalret)
                      (let-parse-evalret* evalret varenv atomenv stdin globalenv
                        (cont expr (evalret*
                                      varenv atomenv stdin
                                      (prepend-envzip-basedata (cons-data varname nil) (cons expr nil) globalenv)))))))
                ;; loc
                (let ((varname (-> tail car-data))
                      (defbody (-> tail cdr-data car-data)))
                  (eval defbody evalret
                    (lambda (expr evalret)
                      (let-parse-evalret* evalret varenv atomenv stdin globalenv
                        (cont expr (evalret*
                                      (prepend-envzip-basedata (cons-data varname nil) (cons expr nil) varenv)
                                      atomenv stdin globalenv))))))
                ;; progn
                (eval-progn tail evalret cont)
                ;; while
                (eval-while (car-data tail) (cdr-data tail) evalret cont)
                ;; lambda
                (cont expr evalret)
                ;; macro
                (cont expr evalret)
                ))))
        ;; list: parse as lambda
        (eval-lambdalike head tail evalret cont)
        ;; nil
        (cont nil evalret)))
    ;; nil
    (cont nil evalret)))

(defrec-lazy repl (varenv atomenv stdin globalenv)
  (cons "*" (cons " " (if (= stringtermchar (car stdin))
    stringterm
    (let ((ret-parse (read-expr stdin atomenv))
          (stdin (car (cdr ret-parse)))
          (atomenv (cdr (cdr ret-parse)))
          (expr (car ret-parse)))
      (eval expr (evalret* varenv atomenv stdin globalenv)
        (lambda (expr evalret)
          (let-parse-evalret* evalret varenv atomenv stdin globalenv
            (let ((outstr (printexpr atomenv expr nil)))
              (await-list outstr
                (append-list outstr (cons "\\n" (repl varenv atomenv stdin globalenv)))))))))))))

(defrec-lazy list2inflist (l)
  (if (isnil l)
    (inflist 256)
    (cons (car l) (list2inflist (cdr l)))))

(defun-lazy main (stdin)
  (repl initial-varenv initial-atomenv stdin nil))


;; (format t (write-to-string (to-de-bruijn (curry (macroexpand-lazy main)))))

(format t (compile-to-ski-lazy main))
;; (format t (compile-to-blc-lazy main))

;; ;; Print lambda term
;; (setf *print-right-margin* 800)
;; (format t (write-to-string (curry (macroexpand-lazy main))))
