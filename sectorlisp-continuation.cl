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

(defun-lazy cmp-lt (x1 x2 x3) x1)
(defun-lazy cmp-eq (x1 x2 x3) x2)
(defun-lazy cmp-gt (x1 x2 x3) x3)
(defrec-lazy cmp-bit (n m)
  (cond ((isnil n)
          cmp-eq)
        ((and (car n) (not (car m)))
          cmp-lt)
        ((and (not (car n)) (car m))
          cmp-gt)
        (t
          (cmp-bit (cdr n) (cdr m)))))

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

(defun-lazy atom* (value)
  (cons type-atom value))

;; (defmacro-lazy list* (arg &rest args)
;;   (if (not args)
;;     `(cons-data ,arg nil)
;;     `(cons-data ,arg (list* ,@args))))

(defrec-lazy printexpr-helper (expr mode cont)
  (if mode
    (typematch expr
      ;; atom
      (do-continuation
        (<- (outstr) (append-list (valueof expr) cont (lambda (x) x)))
        outstr)
      ;; list
      (cons "(" (printexpr-helper expr nil (cons ")" cont)))
      ;; nil
      (cons "N" (cons "I" (cons "L" cont)))
      )
    (do-continuation
      (<- (car-ed) (car-data expr))
      (printexpr-helper car-ed t
        (do-continuation
          (<- (cdr-ed) (cdr-data expr))
          (typematch cdr-ed
            ;; atom
            (cdr-data expr
              (lambda (cdr-ed)
                (cons " " (cons "." (cons " "
                (append-list (valueof cdr-ed) cont (lambda (x) x) (lambda (x) x))
                )))))
            ;; list
            (cdr-data expr
              (lambda (cdr-ed)
                (cons " " (printexpr-helper cdr-ed nil cont))))
            ;; nil
            cont))))))

(defun-lazy printexpr (expr cont)
  (printexpr-helper expr t cont))

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

(defun-lazy read-atom (stdin cont)
  (do-continuation
    (<- (retstr stdin) (read-string nil stdin))
    (cont (atom* retstr) stdin)))

(defrec-lazy stringeq (s1 s2 cont)
  (let ((isnil-s1 (isnil s1))
        (isnil-s2 (isnil s2)))
    (if (and isnil-s1 isnil-s2)
      (cont t)
      (let ((not-isnil-s1 (not isnil-s1))
            (and-1 (and not-isnil-s1 isnil-s2)))
        (if and-1
          (cont nil)
          (let ((not-isnil-s2 (not isnil-s2))
                (and-2 (and isnil-s1 not-isnil-s2)))
            (if and-2
              (cont nil)
              (let ((car-s1 (car s1))
                    (car-s2 (car s2)))
                (if (=-bit car-s1 car-s2)
                  (let ((cdr-s1 (cdr s1))
                        (cdr-s2 (cdr s2)))
                    (stringeq cdr-s1 cdr-s2 cont))
                  (cont nil))))))))))

(defun-lazy stringeq* (s1 s2)
  (stringeq s1 s2 (lambda (x) x)))

(defrec-lazy read-skip-whitespace (stdin cont)
  (let ((c (car stdin)))
    (cond ((or (=-bit " " c) (=-bit "\\n" c))
            (let ((x (cdr stdin)))
              (read-skip-whitespace x cont)))
          (t
            (cont stdin)))))

(defrec-lazy reverse-base2data (l curlist cont)
  (cons-data (car l) curlist
    (lambda (consed)
      (if (isnil l)
        (cont curlist)
        (let ((x (cdr l)))
          (reverse-base2data x consed cont))))))

(defrec-lazy read-expr (stdin curexpr mode cont)
  (read-skip-whitespace stdin
    (lambda (stdin)
        (if mode
          (let ((c (car stdin)))
            (cond ((=-bit ")" c)
                    (do-continuation
                      (<- (reversed) (reverse-base2data curexpr nil))
                      (let* x (cdr stdin))
                      (cont reversed x)))
                  (t
                    (do-continuation
                      (<- (expr stdin) (read-expr stdin nil nil))
                      (let* x (cons expr curexpr))
                      (read-expr stdin x t cont)))))
          (let ((c (car stdin)))
            (cond ((=-bit "(" c)
                    (let ((x (cdr stdin)))
                      (read-expr x nil t cont)))
                  (t
                    (read-atom stdin cont))))))))

(def-lazy maxforms 10)

(def-lazy initial-varenv
  (list
    ;; (cons maxforms (atom* maxforms))
    (cons (list "N" "I" "L") nil)
    ))

(def-lazy t-data
  ;; (atom* maxforms)
  (atom* (list "T"))
  )

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
          ((stringeq varval evarval (lambda (x) x))
            (cont ebody))
          (t
            (let ((varenv-cdr (cdr varenv)))
              (varenv-lookup varenv-cdr varval cont))))))

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

(defun-lazy eval-lambda (lambdaexpr callargs evalret cont)
  (do-continuation
    (<- (lambda-cdr) (cdr-data lambdaexpr))
    (<- (argnames) (car-data lambda-cdr))
    (<- (cdr-ed) (cdr-data lambda-cdr))
    (<- (lambdabody) (car-data cdr-ed))
    (let* varenv-orig (car evalret))
    (<- (argvalues evalret) (eval-map-base callargs nil evalret))
    (let-parse-evalret* evalret varenv stdin globalenv)
    (<- (envzip) (prepend-envzip argnames argvalues varenv nil))
    (<- (new-evalret) (evalret* envzip stdin globalenv))
    (<- (expr evalret) (eval lambdabody new-evalret))
    (let-parse-evalret* evalret varenv stdin globalenv)
    ;; Evaluate the rest of the arguments in the original environment
    (<- (new-evalret) (evalret* varenv-orig stdin globalenv))
    (cont expr new-evalret)))

(defun-lazy evalret* (varenv stdin globalenv cont)
  (cont (list varenv stdin globalenv)))

(defmacro-lazy let-parse-evalret* (evalret varenv stdin globalenv body)
  `(let ((evtmp ,evalret)
         (,varenv      (car evtmp))
         (evtmp (cdr evtmp))
         (,stdin       (car evtmp))
         (evtmp (cdr evtmp))
         (,globalenv   (car evtmp)))
      ,body))

(defrec-lazy eval (expr evalret cont)
  (typematch expr
    ;; atom
    (let-parse-evalret* evalret varenv stdin globalenv
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
    (do-continuation
      (let* evalret-top evalret)
      (<- (head) (car-data expr))
      (let* head-index (valueof head))
      (<- (tail) (cdr-data expr))
      (<- (car-tail) (car-data tail))
      (let* quote-case
        (cont car-tail evalret))
      (let* car-case
        (eval car-tail evalret
          (lambda (expr evalret)
            (car-data expr
              (lambda (car-ed)
                (cont car-ed evalret))))))
      (let* cdr-case
        (eval car-tail evalret
          (lambda (expr evalret)
            (cdr-data expr
              (lambda (cdr-ed)
                (cont cdr-ed evalret))))))
      (let* cons-case
        (eval car-tail evalret
          (lambda (cons-x evalret)
            (cdr-data tail
              (lambda (cdr-ed)
                (car-data cdr-ed
                  (lambda (car-ed)
                    (eval car-ed evalret
                      (lambda (cons-y evalret)
                        (cons-data cons-x cons-y
                          (lambda (consed)
                            (cont consed evalret))))))))))))
      (let* atom-case
        (eval car-tail evalret
          (lambda (expr evalret)
            (truth-data (isatom expr)
              (lambda (truth-ret)
                (cont truth-ret evalret))))))
      (let* eq-case
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
                    (truth-data (stringeq (valueof eq-x) (valueof eq-y) (lambda (x) x))
                      (lambda (x) x))))
            evalret)))
      (let* cond-case
        (eval-cond tail evalret cont))
      (let* print-case
        (if (isnil tail)
          (cons "\\n" (cont nil evalret))
          (eval car-tail evalret
            (lambda (expr evalret)
              (let ((next-text (cont expr evalret)))
                (printexpr expr next-text))))))
      (let* read-case
        (let-parse-evalret* evalret varenv stdin globalenv
          (read-expr stdin nil nil
            (lambda (expr stdin)
              (evalret* varenv stdin globalenv
                (lambda (new-evalret)
                  (cont expr new-evalret)))))))
      (let* def-case
        (do-continuation
          (<- (varname) (car-data tail))
          (<- (cdr-ed) (cdr-data tail))
          (<- (defbody) (car-data cdr-ed))
          (<- (expr evalret) (eval defbody evalret))
          (let-parse-evalret* evalret varenv stdin globalenv)
          (<- (consed) (cons-data varname nil))
          (let* x (cons expr nil))
          (<- (envzip) (prepend-envzip consed x globalenv nil))
          (<- (new-evalret) (evalret* varenv stdin envzip))
          (cont expr new-evalret)))
      (let* default-case
        (eval head evalret
          (lambda (expr evalret)
            (eval-lambda expr tail evalret cont))))
      (let* c-top (car head-index))
      (let* c-tail (cdr head-index))
      (let* cmp-result (cmp-bit c-top "C"))
      (let* cmp-p (cmp-bit c-top "P"))
      (typematch head
        ;; atom
        (cmp-result
          ;; c1 < "C"
          (if (stringeq* head-index (list "A" "T" "O" "M"))
              atom-case
              default-case)
          ;; c1 == "C"
          (if (isnil c-tail)
            default-case
            (let ((c-top (car c-tail))
                  (c-tail2 (cdr c-tail))
                  (cmp-result (cmp-bit c-top "D")))
              (cmp-result
                ;; c2 < "D"
                (if (stringeq* c-tail (list "A" "R"))
                  car-case
                  default-case)
                ;; c2 == "D"
                (if (stringeq* c-tail2 (list "R"))
                  cdr-case
                  default-case)
                ;; c2 > "D"
                (if (=-bit c-top "O")
                  (if (stringeq* c-tail2 (list "N" "S"))
                    cons-case
                    (if (stringeq* c-tail2 (list "N" "D"))
                      cond-case
                      default-case))
                  default-case))))
          ;; c1 > "C"
          (cmp-p
            ;; c1 < "P"
            (if (stringeq* head-index (list "E" "Q"))
              eq-case
              (if (stringeq* head-index (list "D" "E" "F"))
                def-case
                default-case))
            ;; c1 == "P"
            (if (stringeq* c-tail (list "R" "I" "N" "T"))
              print-case
              default-case)
            ;; c1 > "P"
            (if (stringeq* head-index (list "Q" "U" "O" "T" "E"))
              quote-case
              (if (stringeq* head-index (list "R" "E" "A" "D"))
                read-case
                default-case))
            )
          )
        ;; list: parse as lambda
        (eval-lambda head tail evalret-top cont)
        ;; nil
        (cont nil evalret)))
    ;; nil
    (cont nil evalret)))

(defrec-lazy repl (varenv stdin globalenv)
  (cons "*" (cons " "
    (if (isnil (cdr stdin))
      stringterm
      (do-continuation
        (<- (expr stdin) (read-expr stdin nil nil))
        (<- (new-evalret) (evalret* varenv stdin globalenv))
        (<- (expr evalret) (eval expr new-evalret))
        (let-parse-evalret* evalret varenv stdin globalenv)
        (let* repl-next (repl varenv stdin globalenv))
        (let* text-next (cons "\\n" repl-next))
        (printexpr expr text-next))))))

(defrec-lazy list2inflist (l)
  (if (isnil l)
    stringterm
    (cons (car l) (list2inflist (cdr l)))))

(defun-lazy main (stdin)
  (repl initial-varenv stdin nil))


;; (format t (write-to-string (to-de-bruijn (curry (macroexpand-lazy main)))))

;; (format t (compile-to-ski-lazy main))
(format t (compile-to-blc-lazy main))

;; ;; Print lambda term
;; (setf *print-right-margin* 800)
;; (format t (write-to-string (curry (macroexpand-lazy main))))
