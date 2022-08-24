(load "./lazy.cl")


;;================================================================
;; Data structure
;;================================================================
(defun-lazy type-atom (t0 t1) t0)
(defun-lazy type-list (t0 t1) t1)

(defmacro-lazy typeof  (x) `(car ,x))
(defmacro-lazy valueof (x) `(cdr ,x))

(defun-lazy car-data (data cont)
  (cont (car (valueof data))))

(defun-lazy cdr-data (data cont)
  (cont (cdr (valueof data))))

(defun-lazy cons-data (x y cont)
  (cont (cons type-list (cons x y))))

(defun-lazy atom* (value)
  (cons type-atom value))

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


;;================================================================
;; Printing
;;================================================================
(defrec-lazy reverse (l curlist cont)
  (if (isnil l) (cont curlist) (reverse (cdr l) (cons (car l) curlist) cont)))

(defun-lazy append-list (l item cont)
  (do
    (<- (reversed) (reverse l nil))
    (<- (reversed) (reverse reversed item))
    (cont reversed)))

(defun-lazy printatom (expr cont)
  (do
    (<- (outstr) (append-list (valueof expr) cont))
    outstr))

(defrec-lazy printexpr (expr cont)
  (typematch expr
    ;; atom
    (printatom expr cont)
    ;; list
    (cons "(" (printlist expr cont))
    ;; nil
    (cons "N" (cons "I" (cons "L" cont)))))

(defrec-lazy printlist (expr cont)
  (do
    (<- (car-ed) (car-data expr))
    (<- (cdr-ed) (cdr-data expr))
    (printexpr car-ed
      (typematch cdr-ed
        ;; atom
        (cons " " (cons "." (cons " " (printatom cdr-ed (cons ")" cont)))))
        ;; list
        (cons " " (printlist cdr-ed cont))
        ;; nil
        (cons ")" cont)))))


;;================================================================
;; Reader
;;================================================================
(defrec-lazy =-bit (n m)
  (cond ((isnil n)
          t)
        ((xnor (car n) (car m))
          (=-bit (cdr n) (cdr m)))
        (t
          nil)))

(defrec-lazy read-atom* (curstr stdin cont)
  (let ((c (car stdin)))
    (cond
      ((or (=-bit " " c) (=-bit "\\n" c))
        (do
          (<- (reversed) (reverse curstr nil))
          (cont (atom* reversed) (cdr stdin))))
      ((or (=-bit "(" c) (=-bit ")" c))
        (do
          (<- (reversed) (reverse curstr nil))
          (cont (atom* reversed) stdin)))
      (t
        (read-atom* (cons (car stdin) curstr) (cdr stdin) cont)))))

(defun-lazy read-atom (stdin cont)
  (read-atom* nil stdin cont))

(defrec-lazy stringeq (s1 s2)
  (do
    (let* isnil-s1 (isnil s1))
    (let* isnil-s2 (isnil s2))
    (if-then-return (and isnil-s1 isnil-s2) t)
    (if-then-return (and (not isnil-s1) isnil-s2) nil)
    (if-then-return (and isnil-s1 (not isnil-s2)) nil)
    (if-then-return (=-bit (car s1) (car s2))
      (let ((cdr-s1 (cdr s1))
            (cdr-s2 (cdr s2)))
        (stringeq cdr-s1 cdr-s2)))
    nil))

(defmacro-lazy stringeq* (s1 s2)
  `(stringeq ,s1 ,s2))

(defrec-lazy read-skip-whitespace (stdin cont)
  (let ((c (car stdin)))
    (cond ((or (=-bit " " c) (=-bit "\\n" c))
            (read-skip-whitespace (cdr stdin) cont))
          (t
            (cont stdin)))))

(defrec-lazy reverse-base2data (l curlist cont)
  (if (isnil l)
    (cont curlist)
    (do
      (<- (consed) (cons-data (car l) curlist))
      (reverse-base2data (cdr l) consed cont))))

(defrec-lazy read-list (stdin curexpr cont)
  (do
    (cond ((=-bit ")" (car stdin))
          (do
            (<- (reversed) (reverse-base2data curexpr nil))
            (cont reversed (cdr stdin))))
        (t
          (do
            (<- (expr stdin) (read-expr stdin))
            (read-list stdin (cons expr curexpr) cont))))))

(defrec-lazy read-expr (stdin cont)
  (do
    (<- (stdin) (read-skip-whitespace stdin))
    (if-then-return (isnil stdin)
      nil)
    (cond ((=-bit "(" (car stdin))
            (read-list (cdr stdin) nil cont))
          (t
            (read-atom stdin cont)))))


;;================================================================
;; Evaluation helpers
;;================================================================
(defun-lazy new-evalstate (varenv stdin globalenv cont)
  (cont (list varenv stdin globalenv)))

(defmacro-lazy let-parse-evalstate (evalstate varenv stdin globalenv body)
  `(let ((evtmp ,evalstate)
         (,varenv      (car evtmp))
         (evtmp (cdr evtmp))
         (,stdin       (car evtmp))
         (evtmp (cdr evtmp))
         (,globalenv   (car evtmp)))
      ,body))

(defrec-lazy eval-cond (clauselist evalstate cont)
  (do
    (<- (carclause) (car-data clauselist))
    (<- (carcond) (car-data carclause))
    (<- (cdr-ed) (cdr-data carclause))
    (<- (carbody) (car-data cdr-ed))
    (<- (expr evalstate) (eval carcond evalstate))
    (<- (cdr-ed) (cdr-data clauselist))
    (if (or (isnil expr) (stringeq (valueof expr) "NIL"))
      (eval-cond cdr-ed evalstate cont)
      (eval carbody evalstate cont))))

(defrec-lazy varenv-lookup (varenv varval cont)
  (let ((pair (car varenv))
        (evarval (car pair))
        (ebody (cdr pair)))
    (cond ((isnil varenv)
            (let-parse-evalstate evalstate varenv stdin globalenv
              (cons "?" (append-list varval (cons "\\n" (repl varenv stdin globalenv)) (lambda (x) x)))))
          ((stringeq varval evarval)
            (cont ebody))
          (t
            (varenv-lookup (cdr varenv) varval cont)))))

(defrec-lazy eval-map-base (lexpr curexpr evalstate cont)
  (cond ((isnil lexpr)
          (cont curexpr evalstate))
        (t
          (do
            (<- (car-ed) (car-data lexpr))
            (<- (expr evalstate) (eval car-ed evalstate))
            (let* expr-list (cons expr nil))
            (<- (appended) (append-list curexpr expr-list))
            (<- (cdr-ed) (cdr-data lexpr))
            (eval-map-base cdr-ed appended evalstate cont)))))

(defrec-lazy prepend-envzip (argnames evargs env curenv cont)
  (cond ((isnil argnames)
          (do
            (<- (reversed) (reverse curenv nil))
            (<- (appended) (append-list reversed env))
            (cont appended)))
        (t
          (do
            (<- (cdr-ed) (cdr-data argnames))
            (<- (car-ed) (car-data argnames))
            (let* next-evargs (if (isnil evargs) nil (cdr evargs)))
            (let* car-evargs (if (isnil evargs) nil (car evargs)))
            (let* inner-cons (cons (valueof car-ed) car-evargs))
            (let* next-curenv (cons inner-cons curenv))
            (prepend-envzip cdr-ed next-evargs env next-curenv cont)))))

(defun-lazy eval-lambda (lambdaexpr callargs evalstate cont)
  (do
    (<- (lambda-cdr) (cdr-data lambdaexpr))
    (<- (argnames) (car-data lambda-cdr))
    (<- (cdr-ed) (cdr-data lambda-cdr))
    (<- (lambdabody) (car-data cdr-ed))
    (let* varenv-orig (car evalstate))
    (<- (argvalues evalstate) (eval-map-base callargs nil evalstate))
    (let-parse-evalstate evalstate varenv stdin globalenv)
    (<- (envzip) (prepend-envzip argnames argvalues varenv nil))
    (<- (evalstate-new) (new-evalstate envzip stdin globalenv))
    (<- (expr evalstate) (eval lambdabody evalstate-new))
    (let-parse-evalstate evalstate varenv stdin globalenv)
    ;; Evaluate the rest of the arguments in the original environment
    (<- (evalstate-new) (new-evalstate varenv-orig stdin globalenv))
    (cont expr evalstate-new)))


;;================================================================
;; Evaluation
;;================================================================
(def-lazy "QUOTE" (list "Q" "U" "O" "T" "E"))
(def-lazy "CAR" (list "C" "A" "R"))
(def-lazy "CDR" (list "C" "D" "R"))
(def-lazy "CONS" (list "C" "O" "N" "S"))
(def-lazy "ATOM" (list "A" "T" "O" "M"))
(def-lazy "EQ" (list "E" "Q"))
(def-lazy "COND" (list "C" "O" "N" "D"))
(def-lazy "PRINT" (list "P" "R" "I" "N" "T"))
(def-lazy "READ" (list "R" "E" "A" "D"))
(def-lazy "DEF" (list "D" "E" "F"))
(def-lazy "DEFINE" (list "D" "E" "F" "I" "N" "E"))
(def-lazy "AS" (list "A" "S"))
(def-lazy "LAMBDA" (list "L" "A" "M" "B" "D" "A"))
(def-lazy "NIL" (list "N" "I" "L"))

(defun-lazy truth-data (expr)
  (if expr t-data nil))

(def-lazy t-data (atom* (list "T")))

(defrec-lazy eval (expr evalstate cont)
  (typematch expr
    ;; atom
    (let-parse-evalstate evalstate varenv stdin globalenv
      (do
        (let* envlist (if (isnil globalenv) varenv (append-list varenv globalenv (lambda (x) x))))
        (<- (ret) (varenv-lookup envlist (valueof expr)))
        (cont ret evalstate)))
    ;; list
    (do
      (let* evalstate-top evalstate)
      (<- (head) (car-data expr))
      (let* head-index (valueof head))
      (<- (tail) (cdr-data expr))
      (<- (car-tail) (car-data tail))
      (typematch head
        ;; atom
        (do
          (if-then-return (stringeq* head-index "QUOTE")
            (cont car-tail evalstate))

          (if-then-return (stringeq* head-index "CAR")
            (eval car-tail evalstate
              (lambda (expr evalstate)
                (car-data expr
                  (lambda (car-ed)
                    (cont car-ed evalstate))))))

          (if-then-return (stringeq* head-index "CDR")
            (eval car-tail evalstate
              (lambda (expr evalstate)
                (cdr-data expr
                  (lambda (cdr-ed)
                    (cont cdr-ed evalstate))))))

          (if-then-return (stringeq* head-index "CONS")
            (eval car-tail evalstate
              (lambda (cons-x evalstate)
                (cdr-data tail
                  (lambda (cdr-ed)
                    (car-data cdr-ed
                      (lambda (car-ed)
                        (eval car-ed evalstate
                          (lambda (cons-y evalstate)
                            (cons-data cons-x cons-y
                              (lambda (consed)
                                (cont consed evalstate))))))))))))

          (if-then-return (stringeq* head-index "ATOM")
            (eval car-tail evalstate
              (lambda (expr evalstate)
                (cont (truth-data (isatom expr)) evalstate))))

          (if-then-return (stringeq* head-index "EQ")
            (do
              (<- (car-ed) (car-data tail))
              (<- (eq-x evalstate) (eval car-ed evalstate))
              (<- (cdr-ed) (cdr-data tail))
              (<- (car-ed) (car-data cdr-ed))
              (<- (eq-y evalstate) (eval car-ed evalstate))
              (cont
                (cond ((and (or (isnil eq-x) (stringeq (valueof eq-x) "NIL"))
                            (or (isnil eq-y) (stringeq (valueof eq-y) "NIL")))
                        t-data)
                      ((or (isnil eq-x) (isnil eq-y))
                        nil)
                      ((or (not (isatom eq-x)) (not (isatom eq-y)))
                        nil)
                      (t
                        (truth-data (stringeq (valueof eq-x) (valueof eq-y)))))
                evalstate)))

          (if-then-return (stringeq* head-index "COND")
            (eval-cond tail evalstate cont))

          (if-then-return (stringeq* head-index "PRINT")
            (if (isnil tail)
              (cons "\\n" (cont nil evalstate))
              (eval car-tail evalstate
                (lambda (expr evalstate)
                  (let ((next-text (cont expr evalstate)))
                    (printexpr expr next-text))))))

          (if-then-return (stringeq* head-index "READ")
            (let-parse-evalstate evalstate varenv stdin globalenv
              (read-expr stdin
                (lambda (expr stdin)
                  (new-evalstate varenv stdin globalenv
                    (lambda (evalstate-new)
                      (cont expr evalstate-new)))))))

          (if-then-return (stringeq* head-index "DEF")
            (do
              (<- (varname) (car-data tail))
              (<- (cdr-ed) (cdr-data tail))
              (<- (defbody) (car-data cdr-ed))
              (<- (expr evalstate) (eval defbody evalstate))
              (let-parse-evalstate evalstate varenv stdin globalenv)
              (<- (consed) (cons-data varname nil))
              (let* x (cons expr nil))
              (<- (envzip) (prepend-envzip consed x globalenv nil))
              (<- (evalstate-new) (new-evalstate varenv stdin envzip))
              (cont expr evalstate-new)))

          (if-then-return (stringeq* head-index "DEFINE")
            (do
              (<- (varname) (car-data tail))
              (<- (tail) (cdr-data tail))
              (<- (option) (car-data tail))
              (<- (tail) (cdr-data tail))
              (let* defbody
                (if (stringeq* (valueof option) "AS")
                  (cons-data (atom* "LAMBDA") tail (lambda (x) x))
                  (car-data tail (lambda (x) x))))
              (<- (consed) (cons-data varname nil))
              (let* x (cons defbody nil))
              (<- (envzip) (prepend-envzip consed x globalenv nil))
              (<- (evalstate-new) (new-evalstate varenv stdin envzip))
              (cont defbody evalstate-new)))
          ;; Default case
          (do
            (<- (expr evalstate) (eval head evalstate))
            (if-then-return (isnil expr)
              (cont nil evalstate))
            (eval-lambda expr tail evalstate cont)))
        ;; list: parse as lambda
        (eval-lambda head tail evalstate-top cont)
        ;; nil
        (cont nil evalstate)))
    ;; nil
    (cont nil evalstate)))


;;================================================================
;; User interface
;;================================================================
(def-lazy stringterm nil)

(def-lazy initial-globalenv
  (list
    (cons "NIL" nil)))

(defrec-lazy repl (varenv stdin globalenv)
  (cons "*" (cons " "
    (do
      (<- (expr stdin) (read-expr stdin))
      (<- (evalstate-new) (new-evalstate varenv stdin globalenv))
      (<- (expr evalstate) (eval expr evalstate-new))
      (let-parse-evalstate evalstate varenv stdin globalenv)
      (let* repl-next (repl varenv stdin globalenv))
      (let* text-next (cons "\\n" repl-next))
      (printexpr expr text-next)))))


(defun-lazy main (stdin)
  (repl nil stdin initial-globalenv))


;;================================================================
;; Constants
;;================================================================
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
(def-lazy "?" (cons t (cons t (cons nil (cons nil (cons nil (cons nil (cons nil (cons nil nil)))))))))
(def-lazy " " (cons t (cons t (cons nil (cons t (cons t (cons t (cons t (cons t nil)))))))))
(def-lazy "." (cons t (cons t (cons nil (cons t (cons nil (cons nil (cons nil (cons t nil)))))))))
(def-lazy ">" (cons t (cons t (cons nil (cons nil (cons nil (cons nil (cons nil (cons t nil)))))))))
(def-lazy "\\n" (cons t (cons t (cons t (cons t (cons nil (cons t (cons nil (cons t nil)))))))))

(defmacro-lazy if-then-return (condition then else)
  `(if ,condition ,then ,else))

(defmacro-lazy let* (name value body)
  `(let ((,name ,value)) ,body))

(defmacro-lazy do* (top &rest proc)
  (cond ((not proc)
          top)
        ((eq '<- (car (car proc)))
          (let* ((topproc (car proc))
                 (arglist (car (cdr topproc)))
                 (body (car (cdr (cdr topproc)))))
            `(do*
                ,(append body `((lambda ,arglist ,top)))
                ,@(cdr proc))))
        (t
          `(do*
              ,(append (car proc) `(,top))
              ,@(cdr proc)))))

(defmacro-lazy do (&rest proc)
  `(do* ,@(reverse proc)))



;;================================================================
;; Compilation
;;================================================================
;; (format t (write-to-string (to-de-bruijn (curry (macroexpand-lazy main)))))

;; (format t (compile-to-ski-lazy main))
(format t (compile-to-blc-lazy main))

;; ;; Print lambda term
;; (setf *print-right-margin* 800)
;; (format t (write-to-string (curry (macroexpand-lazy main))))

