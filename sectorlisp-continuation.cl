(load "./lazy.cl")

(def-lazy "A" (cons t (cons nil (cons t (cons t (cons t (cons t (cons t (cons nil nil)))))))))
(def-lazy "B" (cons t (cons nil (cons t (cons t (cons t (cons t (cons nil (cons t nil)))))))))
(def-lazy "C" (cons t (cons nil (cons t (cons t (cons t (cons t (cons nil (cons nil nil)))))))))
(def-lazy "D" (cons t (cons nil (cons t (cons t (cons t (cons nil (cons t (cons t nil)))))))))
(def-lazy "E" (cons t (cons nil (cons t (cons t (cons t (cons nil (cons t (cons nil nil)))))))))
(def-lazy "F" (cons t (cons nil (cons t (cons t (cons t (cons nil (cons nil (cons t nil)))))))))
(def-lazy "G" (cons t (cons nil (cons t (cons t (cons t (cons nil (cons nil (cons nil nil)))))))))
(def-lazy "H" (cons t (cons nil (cons t (cons t (cons nil (cons t (cons t (cons t nil)))))))))
(def-lazy "I" (cons t (cons nil (cons t (cons t (cons nil (cons t (cons t (cons nil nil)))))))))
(def-lazy "J" (cons t (cons nil (cons t (cons t (cons nil (cons t (cons nil (cons t nil)))))))))
(def-lazy "K" (cons t (cons nil (cons t (cons t (cons nil (cons t (cons nil (cons nil nil)))))))))
(def-lazy "L" (cons t (cons nil (cons t (cons t (cons nil (cons nil (cons t (cons t nil)))))))))
(def-lazy "M" (cons t (cons nil (cons t (cons t (cons nil (cons nil (cons t (cons nil nil)))))))))
(def-lazy "N" (cons t (cons nil (cons t (cons t (cons nil (cons nil (cons nil (cons t nil)))))))))
(def-lazy "O" (cons t (cons nil (cons t (cons t (cons nil (cons nil (cons nil (cons nil nil)))))))))
(def-lazy "P" (cons t (cons nil (cons t (cons nil (cons t (cons t (cons t (cons t nil)))))))))
(def-lazy "Q" (cons t (cons nil (cons t (cons nil (cons t (cons t (cons t (cons nil nil)))))))))
(def-lazy "R" (cons t (cons nil (cons t (cons nil (cons t (cons t (cons nil (cons t nil)))))))))
(def-lazy "S" (cons t (cons nil (cons t (cons nil (cons t (cons t (cons nil (cons nil nil)))))))))
(def-lazy "T" (cons t (cons nil (cons t (cons nil (cons t (cons nil (cons t (cons t nil)))))))))
(def-lazy "U" (cons t (cons nil (cons t (cons nil (cons t (cons nil (cons t (cons nil nil)))))))))
(def-lazy "V" (cons t (cons nil (cons t (cons nil (cons t (cons nil (cons nil (cons t nil)))))))))
(def-lazy "W" (cons t (cons nil (cons t (cons nil (cons t (cons nil (cons nil (cons nil nil)))))))))
(def-lazy "X" (cons t (cons nil (cons t (cons nil (cons nil (cons t (cons t (cons t nil)))))))))
(def-lazy "Y" (cons t (cons nil (cons t (cons nil (cons nil (cons t (cons t (cons nil nil)))))))))
(def-lazy "Z" (cons t (cons nil (cons t (cons nil (cons nil (cons t (cons nil (cons t nil)))))))))

(def-lazy "(" (cons t (cons t (cons nil (cons t (cons nil (cons t (cons t (cons t nil)))))))))
(def-lazy ")" (cons t (cons t (cons nil (cons t (cons nil (cons t (cons t (cons nil nil)))))))))
(def-lazy "*" (cons t (cons t (cons nil (cons t (cons nil (cons t (cons nil (cons t nil)))))))))
(def-lazy " " (cons t (cons t (cons nil (cons t (cons t (cons t (cons t (cons t nil)))))))))
(def-lazy "." (cons t (cons t (cons nil (cons t (cons nil (cons nil (cons nil (cons t nil)))))))))
(def-lazy ">" (cons t (cons t (cons nil (cons nil (cons nil (cons nil (cons nil (cons t nil)))))))))
(def-lazy "\\n" (cons t (cons t (cons t (cons t (cons nil (cons t (cons nil (cons t nil)))))))))

(defmacro-lazy do-continuation* (top &rest proc)
  (cond ((not proc)
          top)
        ((eq '<- (car (car proc)))
          (let* ((topproc (car proc))
                 (arglist (car (cdr topproc)))
                 (body (car (cdr (cdr topproc)))))
            `(do-continuation*
                ,(append body `((lambda ,arglist ,top)))
                ,@(cdr proc))))
        ((eq 'let* (car (car proc)))
          (let* ((topproc (car proc))
                 (varname (car (cdr topproc)))
                 (body (car (cdr (cdr topproc)))))
            `(do-continuation*
                ,(append `(let ((,varname ,body))) `(,top))
                ,@(cdr proc))))
        (t
          `(do-continuation*
              ,(append (car proc) `(,top))
              ,@(cdr proc)))))

(defmacro-lazy do-continuation (&rest proc)
  `(do-continuation* ,@(reverse proc)))

(defrec-lazy =-bit (n m)
  (cond ((isnil n)
          t)
        ((xnor (car n) (car m))
          (=-bit (cdr n) (cdr m)))
        (t
          nil)))

(def-lazy stringtermchar 256)
(def-lazy stringterm nil)

(def-lazy 3 (succ 2))
(def-lazy 5 (succ 4))
(def-lazy 6 (+ 4 2))
(def-lazy 7 (succ 6))
(def-lazy 9 (succ 8))
(def-lazy 10 (succ 9))
(def-lazy 11 (succ 10))


(defun-lazy append-item-to-stream (stream item)
  (lambda (x) (stream (cons item x))))

(defrec-lazy append-list (l item curstream cont)
  (if (isnil l)
    (cont (curstream item))
    (append-list (cdr l) item (append-item-to-stream curstream (car l)) cont)))


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

(defun-lazy truth-data (expr cont)
  (cont (if expr t-data nil)))

(defun-lazy type-atom (t0 t1) t0)
(defun-lazy type-list (t0 t1) t1)

(def-lazy typeof car*)
(def-lazy valueof cdr*)

(defun-lazy car-data (data cont)
  (cont (car (valueof data))))

(defun-lazy cdr-data (data cont)
  (cont (cdr (valueof data))))

(defun-lazy cons-data (x y cont)
  (cont (cons type-list (cons x y))))

(defmacro-lazy atom* (value)
  `(cons type-atom ,value))

;; (defmacro-lazy list* (arg &rest args)
;;   (if (not args)
;;     `(cons-data ,arg nil)
;;     `(cons-data ,arg (list* ,@args))))

(defun-lazy printatom (atomenv expr cont)
  (append-list (car ((valueof expr) cdr* atomenv)) cont (lambda (x) x)
    (lambda (x) x)))

(defrec-lazy printexpr-helper (atomenv expr mode cont)
  (if mode
    (typematch expr
      ;; atom
      (printatom atomenv expr cont)
      ;; list
      (cons "(" (printexpr-helper atomenv expr nil (cons ")" cont)))
      ;; nil
      (cons "(" (cons ")" cont)))
    (do-continuation
      (<- (car-ed) (car-data expr))
      (printexpr-helper atomenv car-ed t
        (do-continuation
          (<- (cdr-ed) (cdr-data expr))
          (typematch cdr-ed
            ;; atom
            (cdr-data expr
              (lambda (cdr-ed)
                (cons " " (cons "." (cons " " (printatom atomenv cdr-ed cont))))))
            ;; list
            (cdr-data expr
              (lambda (cdr-ed)
                (cons " " (printexpr-helper atomenv cdr-ed nil cont))))
            ;; nil
            cont))))))

(defun-lazy printexpr (atomenv expr cont)
  (printexpr-helper atomenv expr t cont))

(defrec-lazy reverse (l curlist cont)
  (if (isnil l) (cont curlist) (reverse (cdr l) (cons (car l) curlist) cont)))

(defrec-lazy read-string (curstr stdin cont)
  (let ((c (car stdin)))
    (cond
      ((or (=-bit "(" c) (=-bit ")" c) (=-bit " " c) (=-bit "\\n" c))
        (reverse curstr nil
          (lambda (reversed)
            (cont reversed stdin))))
      (t
        (read-string (cons (car stdin) curstr) (cdr stdin) cont)))))

(defun-lazy get-atomindex-env (atomenv str cont)
  ((letrec-lazy get-atomindex-env-helper (cur-atomenv n)
    (cond ((isnil cur-atomenv)
            (append-list atomenv (cons str nil) (lambda (x) x)
              (lambda (appended)
                (cont n appended))))
          (t
            (stringeq (car cur-atomenv) str
              (lambda (p)
                (if p
                  (cont n atomenv)
                  (get-atomindex-env-helper (cdr cur-atomenv) (succ n))))))))
   atomenv 0))

(defun-lazy read-atom (stdin atomenv cont)
  (read-string nil stdin
    (lambda (retstr stdin)
      (get-atomindex-env atomenv retstr
        (lambda (retindex atomenv)
          (cont (atom* retindex) atomenv stdin))))))

(defrec-lazy stringeq (s1 s2 cont)
  (cond ((and (isnil s1) (isnil s2))
          (cont t))
        ((or (and (not (isnil s1)) (isnil s2))
             (and (isnil s1) (not (isnil s2))))
          (cont nil))
        ((=-bit (car s1) (car s2))
          (stringeq (cdr s1) (cdr s2) cont))
        (t
          (cont nil))))

(defrec-lazy read-skip-whitespace (stdin cont)
  (let ((c (car stdin)))
    (cond ((or (=-bit " " c) (=-bit "\\n" c))
            (read-skip-whitespace (cdr stdin) cont))
          (t
            (cont stdin)))))

(defrec-lazy reverse-base2data (l curlist cont)
  (cons-data (car l) curlist
    (lambda (consed)
      (if (isnil l)
        (cont curlist)
        (reverse-base2data (cdr l) consed cont)))))

(defrec-lazy read-expr (stdin atomenv curexpr mode cont)
  (read-skip-whitespace stdin
    (lambda (stdin)
      (let ((c (car stdin)))
        (if mode
          (cond ((=-bit ")" c)
                  (reverse-base2data curexpr nil
                    (lambda (reversed)
                      (cont reversed atomenv (cdr stdin)))))
                (t
                  (read-expr stdin atomenv nil nil
                    (lambda (expr atomenv stdin)
                      (read-expr stdin atomenv (cons expr curexpr) t cont)))))
          (cond ((=-bit "(" c)
                  (read-expr (cdr stdin) atomenv nil t cont))
                (t
                  (read-atom stdin atomenv cont))))))))

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
  (do-continuation
    (<- (carclause) (car-data clauselist))
    (<- (carcond) (car-data carclause))
    (<- (cdr-ed) (cdr-data carclause))
    (<- (carbody) (car-data cdr-ed))
    (<- (expr evalret) (eval carcond evalret))
    (<- (cdr-ed) (cdr-data clauselist))
    (if (isnil expr)
      (eval-cond cdr-ed evalret cont)
      (eval carbody evalret cont))))

(defrec-lazy varenv-lookup (varenv varval cont)
  (let ((pair (car varenv))
        (evarval (car pair))
        (ebody (cdr pair)))
    (cond ((isnil varenv)
            (cont nil))
          ((= varval evarval)
            (cont ebody))
          (t
            (varenv-lookup (cdr varenv) varval cont)))))

(defrec-lazy eval-map-base (lexpr curexpr evalret cont)
  (cond ((isnil lexpr)
          (cont curexpr evalret))
        (t
          (do-continuation
            (<- (car-ed) (car-data lexpr))
            (<- (expr evalret) (eval car-ed evalret))
            (let* expr-list (cons expr nil))
            (<- (appended) (append-list curexpr expr-list (lambda (x) x)))
            (<- (cdr-ed) (cdr-data lexpr))
            (eval-map-base cdr-ed appended evalret cont)))))

(defrec-lazy prepend-envzip (argnames evargs env curenv cont)
  (cond ((isnil argnames)
          (do-continuation
            (<- (reversed) (reverse curenv nil))
            (<- (appended) (append-list reversed env (lambda (x) x)))
            (cont appended)))
        (t
          (do-continuation
            (<- (cdr-ed) (cdr-data argnames))
            (<- (car-ed) (car-data argnames))
            (let* next-evargs (if (isnil evargs) nil (cdr evargs)))
            (let* car-evargs (if (isnil evargs) nil (car evargs)))
            (let* inner-cons (cons (valueof car-ed) car-evargs))
            (let* next-curenv (cons inner-cons curenv))
            (prepend-envzip cdr-ed next-evargs env next-curenv cont)))))

;; (defrec-lazy prepend-envzip (argnames evargs env curenv cont)
;;   (cond ((isnil argnames)
;;           (reverse curenv nil
;;             (lambda (reversed)
;;               (append-list reversed env (lambda (x) x)
;;                 (lambda (appended)
;;                   (cont appended))))))
;;         (t
;;           (cdr-data argnames
;;             (lambda (cdr-ed)
;;               (prepend-envzip cdr-ed (if (isnil evargs) nil (cdr evargs)) env
;;                 (car-data argnames
;;                   (lambda (car-ed)
;;                     (cons (cons (valueof car-ed) (if (isnil evargs) nil (car evargs)))
;;                       curenv)))
;;                 cont))))))

(defun-lazy eval-lambda (lambdaexpr callargs evalret cont)
  (do-continuation
    (<- (lambda-cdr) (cdr-data lambdaexpr))
    (<- (argnames) (car-data lambda-cdr))
    (<- (cdr-ed) (cdr-data lambda-cdr))
    (<- (lambdabody) (car-data cdr-ed))
    (let* varenv-orig (car evalret))
    (<- (argvalues evalret) (eval-map-base callargs nil evalret))
    (let-parse-evalret* evalret varenv atomenv stdin globalenv)
    (<- (envzip) (prepend-envzip argnames argvalues varenv nil))
    (<- (new-evalret) (evalret* envzip atomenv stdin globalenv))
    (<- (expr evalret) (eval lambdabody new-evalret))
    (let-parse-evalret* evalret varenv atomenv stdin globalenv)
    ;; Evaluate the rest of the argument in the original environment
    (<- (new-evalret) (evalret* varenv-orig atomenv stdin globalenv))
    (cont expr new-evalret)))

;; (defmacro-lazy evalret* (varenv atomenv stdin globalenv)
;;   `(list ,varenv ,atomenv ,stdin ,globalenv))

(defun-lazy evalret* (varenv atomenv stdin globalenv cont)
  (cont (list varenv atomenv stdin globalenv)))

(defmacro-lazy let-parse-evalret* (evalret varenv atomenv stdin globalenv body)
  `(let ((evtmp ,evalret)
         (,varenv      (car evtmp))
         (evtmp (cdr evtmp))
         (,atomenv     (car evtmp))
         (evtmp (cdr evtmp))
         (,stdin       (car evtmp))
         (evtmp (cdr evtmp))
         (,globalenv   (car evtmp)))
      ,body))

(defrec-lazy eval (expr evalret cont)
  (typematch expr
    ;; atom
    (let-parse-evalret* evalret varenv atomenv stdin globalenv
      (cond
        ((isnil globalenv)
          (varenv-lookup
            varenv
            (valueof expr)
            (lambda (ret)
              (cont ret evalret))))
        (t
          (append-list varenv globalenv (lambda (x) x)
            (lambda (appended)
              (varenv-lookup
                appended
                (valueof expr)
                (lambda (ret)
                  (cont ret evalret))))))))
    ;; list
    (car-data expr
      (lambda (head)
        (let ((head-index (valueof head)))
          (cdr-data expr
            (lambda (tail)
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
                        (car-data tail
                          (lambda (car-ed)
                            (cont car-ed evalret)))
                        ;; car
                        (car-data tail
                          (lambda (car-ed)
                            (eval car-ed evalret
                              (lambda (expr evalret)
                                (car-data expr
                                  (lambda (car-ed)
                                    (cont car-ed evalret)))))))
                        ;; cdr
                        (car-data tail
                          (lambda (car-ed)
                            (eval car-ed evalret
                              (lambda (expr evalret)
                                (cdr-data expr
                                  (lambda (cdr-ed)
                                    (cont cdr-ed evalret)))))))
                        ;; cons
                        (car-data tail
                          (lambda (car-ed)
                            (eval car-ed evalret
                              (lambda (cons-x evalret)
                                (cdr-data tail
                                  (lambda (cdr-ed)
                                    (car-data cdr-ed
                                      (lambda (car-ed)
                                        (eval car-ed evalret
                                          (lambda (cons-y evalret)
                                            (cons-data cons-x cons-y
                                              (lambda (consed)
                                                (cont consed evalret)))))))))))))
                        ;; atom
                        (car-data tail
                          (lambda (car-ed)
                            (eval car-ed evalret
                              (lambda (expr evalret)
                                (truth-data (isatom expr)
                                  (lambda (truth-ret)
                                    (cont truth-ret evalret)))))))
                        ;; eq
                        (do-continuation
                          (<- (car-ed) (car-data tail))
                          (<- (eq-x evalret) (eval car-ed evalret))
                          (<- (cdr-ed) (cdr-data tail))
                          (<- (car-ed) (car-data cdr-ed))
                          (<- (eq-y evalret) (eval car-ed evalret))
                          (cont
                            (cond ((and (isnil eq-x) (isnil eq-y))
                                    t-data)
                                  ((or (isnil eq-x) (isnil eq-y))
                                    nil)
                                  ((or (not (isatom eq-x)) (not (isatom eq-y)))
                                    nil)
                                  (t
                                    (truth-data (= (valueof eq-x) (valueof eq-y))
                                      (lambda (x) x))))
                            evalret))
                        ;; cond
                        (eval-cond tail evalret cont)
                        ;; print
                        (if (isnil tail)
                          (cons "\\n" (cont nil evalret))
                          (car-data tail
                            (lambda (car-ed)
                              (eval car-ed evalret
                                (lambda (expr evalret)
                                  (let ((atomenv (-> evalret cdr car))
                                        (next-text (cont expr evalret)))
                                    (printexpr atomenv expr next-text)))))))
                        ;; read
                        (let-parse-evalret* evalret varenv-old atomenv stdin globalenv
                          (read-expr stdin atomenv nil nil
                            (lambda (expr atomenv stdin)
                              (evalret* varenv atomenv stdin globalenv
                                (lambda (new-evalret)
                                  (cont expr new-evalret))))))
                        ;; def
                        (do-continuation
                          (<- (varname) (car-data tail))
                          (<- (cdr-ed) (cdr-data tail))
                          (<- (defbody) (car-data cdr-ed))
                          (<- (expr evalret) (eval defbody evalret))
                          (let-parse-evalret* evalret varenv atomenv stdin globalenv)
                          (<- (consed) (cons-data varname nil))
                          (let* x (cons expr nil))
                          (<- (envzip) (prepend-envzip consed x globalenv nil))
                          (<- (new-evalret) (evalret* varenv atomenv stdin envzip))
                          (cont expr new-evalret))))))
                ;; list: parse as lambda
                (let ((lambdaexpr head)
                      (callargs tail))
                  (eval-lambda lambdaexpr callargs evalret cont))
                ;; nil
                (cont nil evalret)))))))
    ;; nil
    (cont nil evalret)))

(defrec-lazy repl (varenv atomenv stdin globalenv)
  (cons "*" (cons " "
    (if (isnil (cdr stdin))
      stringterm
      (do-continuation
        (<- (expr atomenv stdin) (read-expr stdin atomenv nil nil))
        (<- (new-evalret) (evalret* varenv atomenv stdin globalenv))
        (<- (expr evalret) (eval expr new-evalret))
        (let-parse-evalret* evalret varenv atomenv stdin globalenv)
        (let* repl-next (repl varenv atomenv stdin globalenv))
        (let* text-next (cons "\\n" repl-next))
        (printexpr atomenv expr text-next))))))

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
