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
;; Arithmetic and logic
;;================================================================
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

(defun-lazy truth-data (expr)
  (if expr t-data nil))

(def-lazy t-data (atom* (list "T")))


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

(defrec-lazy printexpr-helper (expr mode cont)
  (cond
    (mode
      (typematch expr
        ;; atom
        (do
          (<- (outstr) (append-list (valueof expr) cont))
          outstr)
        ;; list
        (cons "(" (printexpr-helper expr nil (cons ")" cont)))
        ;; nil
        (cons "N" (cons "I" (cons "L" cont)))))
    (t
      (do
        (<- (car-ed) (car-data expr))
        (printexpr-helper car-ed t
          (do
            (<- (cdr-ed) (cdr-data expr))
            (typematch cdr-ed
              ;; atom
              (cdr-data expr
                (lambda (cdr-ed)
                  (cons " " (cons "." (cons " "
                  (append-list (valueof cdr-ed) cont (lambda (x) x))
                  )))))
              ;; list
              (cdr-data expr
                (lambda (cdr-ed)
                  (cons " " (printexpr-helper cdr-ed nil cont))))
              ;; nil
              cont)))))))

(defun-lazy printexpr (expr cont)
  (printexpr-helper expr t cont))


;;================================================================
;; Reader
;;================================================================
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
  (do
    (let* read-string read-string)
    (<- (retstr stdin) (read-string nil stdin))
    (let* retatom (atom* retstr))
    (cont retatom stdin)))

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
  (do
    (<- (stdin) (read-skip-whitespace stdin))
    (let* c (car stdin))
    (let* cdr-stdin (cdr stdin))
    (if mode      
      (cond ((=-bit ")" c)
              (do
                (let* reverse-base2data reverse-base2data)
                (<- (reversed) (reverse-base2data curexpr nil))
                (cont reversed cdr-stdin)))
            (t
              (do
                (<- (expr stdin) (read-expr stdin nil nil))
                (let* x (cons expr curexpr))
                (read-expr stdin x t cont))))
      (cond ((=-bit "(" c)
              (read-expr cdr-stdin nil t cont))
            (t
              (let ((read-atom read-atom))
                (read-atom stdin cont)))))))


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
    (if (isnil expr)
      (eval-cond cdr-ed evalstate cont)
      (eval carbody evalstate cont))))

(defrec-lazy varenv-lookup (varenv varval cont)
  (let ((pair (car varenv))
        (evarval (car pair))
        (ebody (cdr pair)))
    (cond ((isnil varenv)
            (cont nil))
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
(defrec-lazy eval (expr evalstate cont)
  (typematch expr
    ;; atom
    (let-parse-evalstate evalstate varenv stdin globalenv
      (cond
        ((isnil globalenv)
          (varenv-lookup
            varenv
            (valueof expr)
            (lambda (ret)
              (cont ret evalstate))))
        (t
          (append-list varenv globalenv
            (lambda (appended)
              (varenv-lookup
                appended
                (valueof expr)
                (lambda (ret)
                  (cont ret evalstate))))))))
    ;; list
    (do
      (let* evalstate-top evalstate)
      (<- (head) (car-data expr))
      (let* head-index (valueof head))
      (<- (tail) (cdr-data expr))
      (<- (car-tail) (car-data tail))
      (let* quote-case
        (cont car-tail evalstate))
      (let* car-case
        (eval car-tail evalstate
          (lambda (expr evalstate)
            (car-data expr
              (lambda (car-ed)
                (cont car-ed evalstate))))))
      (let* cdr-case
        (eval car-tail evalstate
          (lambda (expr evalstate)
            (cdr-data expr
              (lambda (cdr-ed)
                (cont cdr-ed evalstate))))))
      (let* cons-case
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
      (let* atom-case
        (eval car-tail evalstate
          (lambda (expr evalstate)
            (cont (truth-data (isatom expr)) evalstate))))
      (let* eq-case
        (do
          (<- (car-ed) (car-data tail))
          (<- (eq-x evalstate) (eval car-ed evalstate))
          (<- (cdr-ed) (cdr-data tail))
          (<- (car-ed) (car-data cdr-ed))
          (<- (eq-y evalstate) (eval car-ed evalstate))
          (cont
            (cond ((and (isnil eq-x) (isnil eq-y))
                    t-data)
                  ((or (isnil eq-x) (isnil eq-y))
                    nil)
                  ((or (not (isatom eq-x)) (not (isatom eq-y)))
                    nil)
                  (t
                    (truth-data (stringeq (valueof eq-x) (valueof eq-y)))))
            evalstate)))
      (let* cond-case
        (eval-cond tail evalstate cont))
      (let* print-case
        (if (isnil tail)
          (cons "\\n" (cont nil evalstate))
          (eval car-tail evalstate
            (lambda (expr evalstate)
              (let ((next-text (cont expr evalstate)))
                (printexpr expr next-text))))))
      (let* read-case
        (let-parse-evalstate evalstate varenv stdin globalenv
          (read-expr stdin nil nil
            (lambda (expr stdin)
              (new-evalstate varenv stdin globalenv
                (lambda (evalstate-new)
                  (cont expr evalstate-new)))))))
      (let* def-case
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
      (let* default-case
        (eval head evalstate
          (lambda (expr evalstate)
            (eval-lambda expr tail evalstate cont))))
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
                default-case))))
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

(def-lazy initial-varenv
  (list
    (cons (list "N" "I" "L") nil)))

(defrec-lazy repl (varenv stdin globalenv)
  (cons "*" (cons " "
    (if (isnil (cdr stdin))
      stringterm
      (do
        (<- (expr stdin) (read-expr stdin nil nil))
        (<- (evalstate-new) (new-evalstate varenv stdin globalenv))
        (<- (expr evalstate) (eval expr evalstate-new))
        (let-parse-evalstate evalstate varenv stdin globalenv)
        (let* repl-next (repl varenv stdin globalenv))
        (let* text-next (cons "\\n" repl-next))
        (printexpr expr text-next))))))


(defun-lazy main (stdin)
  (repl initial-varenv stdin nil))


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

