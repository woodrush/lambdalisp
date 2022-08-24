(load "./lazy.cl")


(defrec-lazy Evcon (c a cont)
  (do
    (<- (x) (car-data c))
    (<- (x) (car-data x))
    (<- (expr) (Eval x a))
    (cond
      ((isnil expr)
        (do
          (<- (x) (cdr-data c))
          (Evcon x a cont)))
      (t
        (do
          (<- (x) (car-data c))
          (<- (x) (cdr-data x))
          (<- (x) (car-data x))
          (Eval x a cont))))))

(defrec-lazy Evlis (m a cont)
  (cond
    ((isnil m)
      (cont m))
    (t
      (do
        (<- (x) (car-data m))
        (<- (x) (Eval x a))
        (<- (y) (cdr-data m))
        (<- (y) (Evlis y a))
        (cons-data x y cont)
        ))))

(defrec-lazy Assoc (x y cont)
  (do
    (<- (a) (car-data y))
    (<- (a) (car-data y))
    (cond
      ((stringeq x a)
        (do
          (<- (a) (car-data y))
          (cdr-data a cont)))
      (t
        (do
          (<- (a) (cdr-data y))
          (Assoc x a cont))))))

(defrec-lazy Pairlis (x y a cont)
  (cond
    ((isnil x)
      a)
    (t
      (do
        (<- (p) (car-data x))
        (<- (q) (car-data y))
        (<- (r) (cons-data p q))
        (<- (f) (cdr-data x))
        (<- (g) (cdr-data y))
        (<- (h) (Pairlis f g a))
        (cons-data r h cont)))))

(defrec-lazy Eval (e a cont)
  (cond
    ((isnil e)
      (cont e))
    ((isatom e)
      (Assoc e a cont))
    (t
      (do
        (<- (x) (car-data e))
        (cond
          ((stringeq (valueof x) kQuote)
            (do
              (<- (x) (cdr-data e))
              (car-data x cont)))
          ((stringeq (valueof x) kCond)
            (do
              (<- (x) (cdr-data e))
              (Evcon x a cont)))
          (t
            (do
              (<- (x) (car-data e))
              (<- (y) (cdr-data e))
              (<- (y) (Evlis y a))
              (Apply x y a cont)
              )
              )
              ))
              )))

(defrec-lazy Apply (f x a cont)
  (cond
    ((isatom f)
      (let ((fv (valueof f)))
        (cond
          ((stringeq fv kEq)
            (do
              (<- (p) (car-data x))
              (<- (q) (cdr-data x))
              (<- (q) (car-data x))
              (let* ret (and (isatom p) (and (isatom q) (stringeq (valueof p) (valueof q)))))
              (cont ret)))
          ((stringeq fv kCons)
            (do
              (<- (p) (car-data x))
              (<- (q) (cdr-data x))
              (<- (q) (car-data q))
              (cons-data p q cont)))
          ((stringeq fv kAtom)
            (do
              (<- (p) (car-data x))
              (let* ret (if (isatom p) t-atom nil))
              (cont ret)))
          ((stringeq fv kCar)
            (do
              (<- (p) (car-data x))
              (car-data p cont)))
          ((stringeq fv kCdr)
            (do
              (<- (p) (cdr-data x))
              (car-data p cont)))
          (t
            (do
              (<- (p) (Assoc f a))
              (Apply p x a cont))))))
    (t
      (do
        (<- (cdr-f) (cdr-data f))
        (<- (p) (cdr-data cdr-f))
        (<- (p) (car-data p))
        (<- (q) (car-data cdr-f))
        (<- (q) (Pairlis q x a))
        (Eval p q cont)))))


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

(defun-lazy isnil-data (expr)
  (typematch expr
    ;; atom
    nil
    ;; list
    nil
    ;; nil
    t))

(def-lazy kQuote (list "Q" "U" "O" "T" "E"))
(def-lazy kCar (list "C" "A" "R"))
(def-lazy kCdr (list "C" "D" "R"))
(def-lazy kCons (list "C" "O" "N" "S"))
(def-lazy kAtom (list "A" "T" "O" "M"))
(def-lazy kEq (list "E" "Q"))
(def-lazy kCond (list "C" "O" "N" "D"))
(def-lazy kNil (list "N" "I" "L"))
(def-lazy kT (list "T"))

(def-lazy t-atom (atom* kT))

;; (def-lazy "PRINT" (list "P" "R" "I" "N" "T"))
;; (def-lazy "READ" (list "R" "E" "A" "D"))
;; (def-lazy "DEF" (list "D" "E" "F"))
;; (def-lazy "DEFINE" (list "D" "E" "F" "I" "N" "E"))
;; (def-lazy "AS" (list "A" "S"))
;; (def-lazy "LAMBDA" (list "L" "A" "M" "B" "D" "A"))

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
;; User interface
;;================================================================
(def-lazy stringterm nil)

(defrec-lazy repl (x)
  (cons "*" (cons " "
    (do
      (<- (expr stdin) (read-expr stdin))
      (<- (expr) (Eval expr nil))
      (printexpr expr nil)

      ;; (let-parse-evalstate evalstate varenv stdin globalenv)
      ;; (let* repl-next (repl varenv stdin globalenv))
      ;; (let* text-next (cons "\\n" repl-next))
      ;; (printexpr expr text-next)
      ))))


(defun-lazy main (stdin)
  (repl nil))


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

