(load "./lazy.cl")


(def-lazy stringtermchar 256)
(def-lazy stringterm nil)

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


(defrec-lazy map (f l)
  (if (isnil l)
    nil
    (cons (f (car l)) (map f (cdr l)))))

(defrec-lazy length (l)
  ((letrec-lazy length (l n)
      (if (isnil l)
        n
        (length (cdr l) (succ n))))
    l 0))

(defmacro-lazy await-list (l body)
  `(if (<= 0 (length ,l))
    ,body
    ,body))

(defrec-lazy append-element (l item)
  (if (isnil l) (cons item nil) (cons (car l) (append-element (cdr l) item))))

(defrec-lazy append-list (l item)
  (if (isnil l) item (cons (car l) (append-list (cdr l) item))))

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

(defun-lazy truth-data (expr)
  (if expr t-data nil))

(defun-lazy type-atom (t0 t1) t0)
(defun-lazy type-list (t0 t1) t1)

(def-lazy typeof car*)
(def-lazy valueof cdr*)

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
  (append-list (car ((valueof expr) cdr* atomenv)) cont))

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

;; (defrec-lazy reverse (l curlist)
;;   (if (isnil l) curlist (reverse (cdr l) (cons (car l) curlist))))

(defrec-lazy reverse (l curlist)
  (if (isnil l) curlist (reverse (cdr l) (cons (car l) curlist))))

;; (defmacro-lazy reverse (l)
;;   `(reverse-helper ,l nil))

(defrec-lazy read-string (curstr stdin)
  (let ((c (car stdin)))
    (cond
          ((or (= "(" c) (= ")" c) (= " " c) (= "\\n" c)
          ;; (= stringtermchar c)
          )
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
    (cond ((or (= " " c) (= "\\n" c)
    ;; (= stringtermchar c)
    )
            (read-skip-whitespace (cdr stdin)))
          (t
            stdin))))

(defrec-lazy reverse-base2data (l curlist)
  (if (isnil l) curlist (reverse-base2data (cdr l) (cons-data (car l) curlist))))

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
    (list "Q" "U" "O" "T" "E")
    (list "C" "A" "R")
    (list "C" "D" "R")
    (list "C" "O" "N" "S")
    (list "A" "T" "O" "M")
    (list "E" "Q")
    (list "C" "O" "N" "D")
    (list "P" "R" "I" "N" "T")
    (list "R" "E" "A" "D")
    (list "D" "E" "F")
    (list "T")))

(def-lazy maxforms 10)

(def-lazy initial-varenv
  (list (cons maxforms (atom* maxforms))))

(def-lazy t-data
  (atom* maxforms))

(defrec-lazy eval-cond (clauselist evalret cont)
  (let ((carclause (-> clauselist car-data))
        (carcond (-> carclause car-data))
        (carbody (-> carclause cdr-data car-data)))
    (eval carcond evalret
      (lambda (expr evalret)
        (if (isnil expr)
          (eval-cond (cdr-data clauselist) evalret cont)
          (eval carbody evalret cont))))))

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

(defrec-lazy prepend-envzip (argnames evargs env)
  (cond ((isnil argnames)
          env)
        (t
          (cons (cons (valueof (car-data argnames)) (if (isnil evargs) nil (car evargs)))
                (prepend-envzip (cdr-data argnames) (if (isnil evargs) nil (cdr evargs)) env)))))

(defun-lazy eval-lambda (lambdaexpr callargs evalret cont)
  (let ((argnames (-> lambdaexpr cdr-data car-data))
        (lambdabody (-> lambdaexpr cdr-data cdr-data car-data))
        (varenv-orig (car evalret)))
    (eval-map-base callargs nil evalret
      (lambda (argvalues evalret)
        (let-parse-evalret* evalret varenv atomenv stdin globalenv
          (eval
            lambdabody
            (evalret* (prepend-envzip argnames argvalues varenv) atomenv stdin globalenv)
            (lambda (expr evalret)
              (let-parse-evalret* evalret varenv atomenv stdin globalenv
                ;; Evaluate the rest of the argument in the original environment
                (cont expr (evalret* varenv-orig atomenv stdin globalenv))))))))))

(defmacro-lazy evalret* (varenv atomenv stdin globalenv)
  `(list ,varenv ,atomenv ,stdin ,globalenv))

(defmacro-lazy let-parse-evalret* (evalret varenv atomenv stdin globalenv body)
  `(let ((,varenv      (-> ,evalret car))
         (,atomenv     (-> ,evalret cdr car))
         (,stdin       (-> ,evalret cdr cdr car))
         (,globalenv   (-> ,evalret cdr cdr cdr car)))
      ,body))

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
          ((<= maxforms head-index)
            (eval head evalret
              (lambda (expr evalret)
                (eval-lambda expr tail evalret cont))))
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
                                      (prepend-envzip (cons-data varname nil) (cons expr nil) globalenv)))))))))))
        ;; list: parse as lambda
        (let ((lambdaexpr head)
              (callargs tail))
          (eval-lambda lambdaexpr callargs evalret cont))
        ;; nil
        (cont nil evalret)))
    ;; nil
    (cont nil evalret)))

(defrec-lazy repl (varenv atomenv stdin globalenv)
  (cons "*" (cons " " (if (isnil (cdr stdin)) ;(= stringtermchar (car stdin))
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
    stringterm
    (cons (car l) (list2inflist (cdr l)))))

(defun-lazy main (stdin)
  (repl initial-varenv initial-atomenv stdin nil))


;; (format t (write-to-string (to-de-bruijn (curry (macroexpand-lazy main)))))

;; (format t (compile-to-ski-lazy main))
(format t (compile-to-blc-lazy main))

;; ;; Print lambda term
;; (setf *print-right-margin* 800)
;; (format t (write-to-string (curry (macroexpand-lazy main))))
