(load "./lazy.cl")


(defrec-lazy Evcon (c a cont)
  (do
    (<- (car-c) (car-data c))
    (<- (x) (car-data car-c))
    (<- (expr) (Eval x a))
    (cond
      ((isnil expr)
        (do
          (<- (x) (cdr-data c))
          (Evcon x a cont)))
      (t
        (do
          (<- (x) (cdr-data car-c))
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
        (cons-data x y cont)))))

(defrec-lazy Assoc (x y cont)
  (do
    (<- (car-y) (car-data y))
    (<- (caar-y) (car-data car-y))
    (cond
      ((stringeq (valueof x) (valueof caar-y))
        (do
          (cdr-data car-y cont)))
      (t
        (do
          (<- (cdaar-y) (cdr-data y))
          (Assoc x cdaar-y cont))))))

(defrec-lazy Pairlis (x y a cont)
  (cond
    ((isnil x)
      (cont a))
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
        (<- (car-e) (car-data e))
        (<- (cdr-e) (cdr-data e))
        (let* val-e (valueof car-e))
        (cond
          ((stringeq val-e kQuote)
            (do
              (car-data cdr-e cont)))
          ((stringeq val-e kCond)
            (do
              (Evcon cdr-e a cont)))
          (t
            (do
              (<- (y) (Evlis cdr-e a))
              (Apply car-e y a cont))))))))

(defrec-lazy Apply (f x a cont)
  (cond
    ((isatom f)
      (do
        (<- (car-x) (car-data x))
        (let* fv (valueof f))
        (cond
          ((stringeq fv kEq)
            (do
              (<- (q) (cdr-data x))
              (<- (q) (car-data q))
              (if-then-return (not (and (isatom car-x) (isatom q)))
                (cont nil))
              (let* p-val (if (isnil car-x) kNil (valueof car-x)))
              (let* q-val (if (isnil q) kNil (valueof q)))
              (let* ret (if (stringeq p-val q-val) t-atom nil))
              (cont ret)))
          ((stringeq fv kCons)
            (do
              (<- (q) (cdr-data x))
              (<- (q) (car-data q))
              (cons-data car-x q cont)))
          ((stringeq fv kAtom)
            (do
              (let* ret (if (isatom car-x) t-atom nil))
              (cont ret)))
          ((stringeq fv kCar)
            (do
              (car-data car-x cont)))
          ((stringeq fv kCdr)
            (do
              (cdr-data car-x cont)))
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
      ((or (=-bit "(" c) (=-bit ")" c) (=-bit " " c) (=-bit "\\n" c))
        (do
          (<- (reversed) (reverse curstr nil))
          (if-then-return (stringeq reversed kNil)
            (cont nil stdin))
          (cont (atom* reversed) stdin)))
      (t
        (do
          (read-atom* (cons (car stdin) curstr) (cdr stdin) cont))))))

(defun-lazy read-atom (stdin cont)
  (read-atom* nil stdin cont))

(defrec-lazy stringeq (s1 s2)
  (do
    (let* isnil-s1 (isnil s1))
    (let* isnil-s2 (isnil s2))
    (if-then-return (and isnil-s1 isnil-s2) t)
    (if-then-return (or isnil-s1 isnil-s2) nil)
    (if-then-return (=-bit (car s1) (car s2))
      (stringeq (cdr s1) (cdr s2)))
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
(defrec-lazy main (stdin)
  (do
    (let* car-data car-data)
    (let* cdr-data cdr-data)
    (let* cons-data cons-data)
    (let* stringeq stringeq)
    (let* t-atom t-atom)
    (<- (expr stdin) (read-expr stdin))
    (<- (expr) (Eval expr nil))
    (printexpr expr (cons "\\n" (main stdin)))))


;;================================================================
;; Constants
;;================================================================
(def-lazy kQuote (list "Q" "U" "O" "T" "E"))
(def-lazy kCar   (list "C" "A" "R"))
(def-lazy kCdr   (list "C" "D" "R"))
(def-lazy kCons  (list "C" "O" "N" "S"))
(def-lazy kAtom  (list "A" "T" "O" "M"))
(def-lazy kEq    (list "E" "Q"))
(def-lazy kCond  (list "C" "O" "N" "D"))
(def-lazy kNil   (list "N" "I" "L"))
(def-lazy kT     (list "T"))


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
(def-lazy " " (cons t (cons t (cons nil (cons t (cons t (cons t (cons t (cons t nil)))))))))
(def-lazy "." (cons t (cons t (cons nil (cons t (cons nil (cons nil (cons nil (cons t nil)))))))))
(def-lazy "\\n"
              (cons t (cons t (cons t (cons t (cons nil (cons t (cons nil (cons t nil)))))))))

(def-lazy "*" (cons t (cons t (cons nil (cons t (cons nil (cons t (cons nil (cons t nil)))))))))
(def-lazy "?" (cons t (cons t (cons nil (cons nil (cons nil (cons nil (cons nil (cons nil nil)))))))))
(def-lazy ">" (cons t (cons t (cons nil (cons nil (cons nil (cons nil (cons nil (cons t nil)))))))))

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

