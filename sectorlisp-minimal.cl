(load "./lazy.cl")


(defrec-lazy Evcon (c a cont)
  (do
    (<- (car-c) (car-data c))
    (<- (x) (car-data car-c))
    (<- (expr) (Eval x a))
    (cond
      ((isnil-data expr)
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
    ((isnil-data m)
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
        (cdr-data car-y cont))
      (t
        (do
          (<- (cdaar-y) (cdr-data y))
          (Assoc x cdaar-y cont))))))

(defrec-lazy Pairlis (x y a cont)
  (cond
    ((isnil-data x)
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
    ((isnil-data e)
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
            (car-data cdr-e cont))
          ((stringeq val-e kCond)
            (Evcon cdr-e a cont))
          (t
            (do
              (<- (y) (Evlis cdr-e a))
              (Apply car-e y a cont))))))))

(defrec-lazy Apply (f x a cont)
  (cond
    ((isatom f)
      (do
        (<- (arg2) (cdr-data x))
        (<- (arg2) (car-data arg2))        
        (<- (car-x) (car-data x))
        (let* fv (valueof f))
        (cond
          ((stringeq fv kEq)
            (do
              (if-then-return (not (and (isatom car-x) (isatom arg2)))
                (cont (atom* nil)))
              (let* p-val (valueof car-x))
              (let* q-val (valueof arg2))
              (let* ret (if (stringeq p-val q-val) t-atom (atom* nil)))
              (cont ret)))
          ((stringeq fv kCons)
            (cons-data car-x arg2 cont))
          ((stringeq fv kAtom)
            (do
              (let* ret (if (isatom car-x) t-atom (atom* nil)))
              (cont ret)))
          ((stringeq fv kCar)
            (car-data car-x cont))
          ((stringeq fv kCdr)
            (cdr-data car-x cont))
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

(defmacro-lazy typematch (expr atomcase listcase)
  `((typeof ,expr)
    ,atomcase
    ,listcase))

(defmacro-lazy isatom (expr)
  `(typeof ,expr))

(defmacro-lazy isnil-data (expr)
  `(isnil (valueof ,expr)))

(defrec-lazy add-carry (n m carry invert)
  (cond
    ((isnil n)
      nil)
    (t
      (let ((next (lambda (x y) (cons x (add-carry (cdr n) (cdr m) y invert))))
            (diff (next (not carry) carry)))
        (if (xor invert (car m))
          (if (car n)
            (next carry t)
            diff)
          (if (car n)
            diff
            (next carry nil)))))))

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
;; (def-lazy nil-atom (atom* nil))

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
    (if (isnil-data expr)
      (cons "N" (cons "I" (cons "L" cont)))
      (printatom expr cont))
    ;; list
    (cons "(" (printlist expr cont))))

(defrec-lazy printlist (expr cont)
  (do
    (<- (car-ed) (car-data expr))
    (<- (cdr-ed) (cdr-data expr))
    (printexpr car-ed
      (typematch cdr-ed
        ;; atom
        (if (isnil-data cdr-ed)
          (cons ")" cont)
          (cons " " (cons "." (cons " " (printatom cdr-ed (cons ")" cont))))))
        ;; list
        (cons " " (printlist cdr-ed cont))))))


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
            (cont (atom* nil) stdin))
          (cont (atom* reversed) stdin)))
      (t
        (do
          (read-atom* (cons (car stdin) curstr) (cdr stdin) cont))))))

(defmacro-lazy read-atom (stdin cont)
  `(read-atom* nil ,stdin ,cont))

(defrec-lazy stringeq (s1 s2)
  (do
    (let* isnil-s1 (isnil s1))
    (let* isnil-s2 (isnil s2))
    (if-then-return (and isnil-s1 isnil-s2) t)
    (if-then-return (or isnil-s1 isnil-s2) nil)
    (if-then-return (=-bit (car s1) (car s2))
      (stringeq (cdr s1) (cdr s2)))
    nil))

(defrec-lazy reverse-base2data (l curlist cont)
  (if (isnil l)
    (cont curlist)
    (do
      (<- (consed) (cons-data (car l) curlist))
      (reverse-base2data (cdr l) consed cont))))

(defrec-lazy read-list (stdin curexpr cont)
  (do
    (cond
      ((=-bit ")" (car stdin))
        (do
          (<- (reversed) (reverse-base2data curexpr (atom* nil)))
          (cont reversed (cdr stdin))))
      (t
        (do
          (<- (expr stdin) (read-expr stdin))
          (read-list stdin (cons expr curexpr) cont))))))

(defrec-lazy read-expr (stdin cont)
  (do
    (let* c (car stdin))
    (cond
      ((or (=-bit " " c) (=-bit "\\n" c))
        (read-expr (cdr stdin) cont))
      ((=-bit "(" c)
        (read-list (cdr stdin) nil cont))
      (t
        (read-atom stdin cont)))))


;;================================================================
;; User interface
;;================================================================
(defrec-lazy main (stdin)
  (alphabet-env
    (do
      (let* prefix-t-nil prefix-t-nil)
      (let* alphabet-prefix-nil alphabet-prefix-nil)
      (let* alphabet-prefix-t alphabet-prefix-t)
      (let* "I" (alphabet-prefix-t "I-tail"))
      (let* "L" (alphabet-prefix-t "L-tail"))
      (let* "M" (alphabet-prefix-t "M-tail"))
      (let* "S" (alphabet-prefix-nil "S-tail"))
      (let* "U" (alphabet-prefix-nil "U-tail"))
      (let* "A" (alphabet-prefix-t "A-tail"))
      (let* "D" (alphabet-prefix-t "D-tail"))
      (let* "E" (alphabet-prefix-t "E-tail"))
      (let* "Q" (alphabet-prefix-nil "Q-tail"))
      (let* "R" (alphabet-prefix-nil "R-tail"))
      (let* "N" (alphabet-prefix-t "N-tail"))
      (let* "T" (alphabet-prefix-nil "T-tail"))
      (let* "C" (alphabet-prefix-t "C-tail"))
      (let* "O" (alphabet-prefix-t "O-tail"))
      (let* kQuote (list "Q" "U" "O" "T" "E"))
      (let* kCar   (list "C" "A" "R"))
      (let* kCdr   (list "C" "D" "R"))
      (let* kAtom  (list "A" "T" "O" "M"))
      (let* kEq    (list "E" "Q"))
      (let* prefix-CON (lambda (x) (cons "C" (cons "O" (cons "N" x)))))
      (let* kCons  (prefix-CON (list "S")))
      (let* kCond  (prefix-CON (list "D")))
      (let* kNil   (list "N" "I" "L"))
      (let* t-atom t-atom)
      (let* cons-data cons-data)
      (let* stringeq stringeq)
      (let* cdr-data cdr-data)
      (let* car-data car-data)
      (<- (expr stdin) (read-expr stdin))
      (<- (expr) (Eval expr nil))
      (printexpr expr (cons "\\n" (main stdin))))))


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

;; (def-lazy "U" (cons nil (2 (lambda (x) (cons t (cons nil x))) nil)))
;; (defrec-lazy inc-bit (n cont)
;;   (cond
;;     ((isnil n)
;;       (cont nil t))
;;     (t
;;       (do
;;         (let* n-cdr (cdr n))
;;         (let* a n-cdr)
;;         (<- (n-cdr-inc carry) (inc-bit a))
;;         (let* n-car (car n))
;;         (let* b (xor n-car carry))
;;         (let* c (and n-car carry))
;;         (let* ret (cons b n-cdr-inc))
;;         (cont ret c)
;;         ))))

;; (defun-lazy inc-bit-prefix (n cont)
;;   (do
;;     (<- (n-inc carry) (inc-bit n))
;;     (let* x (cons t n-inc))
;;     (let* x (cons nil x))
;;     (let* x (cons t x))
;;     (cont x n-inc)
;;     ))

;; (defmacro-lazy alphabet-env (body)
;;   `(do
;;     (<- ("T" m) (inc-bit-prefix "U"))
;;     (<- ("S" m) (inc-bit-prefix m))
;;     (<- ("R" m) (inc-bit-prefix m))
;;     (<- ("Q" m) (inc-bit-prefix m))
;;     (<- ("P" m) (inc-bit-prefix m))
;;     (<- ("O" m) (inc-bit-prefix m))
;;     (<- ("N" m) (inc-bit-prefix m))
;;     (<- ("M" m) (inc-bit-prefix m))
;;     (<- ("L" m) (inc-bit-prefix m))
;;     (<- ("K" m) (inc-bit-prefix m))
;;     (<- ("J" m) (inc-bit-prefix m))
;;     (<- ("I" m) (inc-bit-prefix m))
;;     (<- ("H" m) (inc-bit-prefix m))
;;     (<- ("G" m) (inc-bit-prefix m))
;;     (<- ("F" m) (inc-bit-prefix m))
;;     (<- ("E" m) (inc-bit-prefix m))
;;     (<- ("D" m) (inc-bit-prefix m))
;;     (<- ("C" m) (inc-bit-prefix m))
;;     (<- ("B" m) (inc-bit-prefix m))
;;     (<- ("A" m) (inc-bit-prefix m))
;;     (let* "U" (cons t (cons nil (cons t "U"))))
;;     ,body))
(defmacro-lazy alphabet-env (body) body)
(defun-lazy prefix-t-nil (x) (cons t (cons nil x)))
(defun-lazy alphabet-prefix-t (x) (prefix-t-nil (cons t (cons t x))))
(defun-lazy alphabet-prefix-nil (x) (2 prefix-t-nil x))
(def-lazy "A" (alphabet-prefix-t "A-tail"))
(def-lazy "B" (alphabet-prefix-t "B-tail"))
(def-lazy "C" (alphabet-prefix-t "C-tail"))
(def-lazy "D" (alphabet-prefix-t "D-tail"))
(def-lazy "E" (alphabet-prefix-t "E-tail"))
(def-lazy "F" (alphabet-prefix-t "F-tail"))
(def-lazy "G" (alphabet-prefix-t "G-tail"))
(def-lazy "H" (alphabet-prefix-t "H-tail"))
(def-lazy "I" (alphabet-prefix-t "I-tail"))
(def-lazy "J" (alphabet-prefix-t "J-tail"))
(def-lazy "K" (alphabet-prefix-t "K-tail"))
(def-lazy "L" (alphabet-prefix-t "L-tail"))
(def-lazy "M" (alphabet-prefix-t "M-tail"))
(def-lazy "N" (alphabet-prefix-t "N-tail"))
(def-lazy "O" (alphabet-prefix-t "O-tail"))
(def-lazy "P" (alphabet-prefix-nil "P-tail"))
(def-lazy "Q" (alphabet-prefix-nil "Q-tail"))
(def-lazy "R" (alphabet-prefix-nil "R-tail"))
(def-lazy "S" (alphabet-prefix-nil "S-tail"))
(def-lazy "T" (alphabet-prefix-nil "T-tail"))
(def-lazy "U" (alphabet-prefix-nil "U-tail"))
(def-lazy "V" (alphabet-prefix-nil "V-tail"))
(def-lazy "W" (alphabet-prefix-nil "W-tail"))
(def-lazy "X" (alphabet-prefix-nil "X-tail"))
(def-lazy "Y" (alphabet-prefix-nil "Y-tail"))
(def-lazy "Z" (alphabet-prefix-nil "Z-tail"))

(def-lazy "A-tail" (cons t (cons t (cons t (cons nil nil)))))
(def-lazy "B-tail" (cons t (cons t (cons nil (cons t nil)))))
(def-lazy "C-tail" (cons t (cons t (cons nil (cons nil nil)))))
(def-lazy "D-tail" (cons t (cons nil (cons t (cons t nil)))))
(def-lazy "E-tail" (cons t (cons nil (cons t (cons nil nil)))))
(def-lazy "F-tail" (cons t (cons nil (cons nil (cons t nil)))))
(def-lazy "G-tail" (cons t (cons nil (cons nil (cons nil nil)))))
(def-lazy "H-tail" (cons nil (cons t (cons t (cons t nil)))))
(def-lazy "I-tail" (cons nil (cons t (cons t (cons nil nil)))))
(def-lazy "J-tail" (cons nil (cons t (cons nil (cons t nil)))))
(def-lazy "K-tail" (cons nil (cons t (cons nil (cons nil nil)))))
(def-lazy "L-tail" (cons nil (cons nil (cons t (cons t nil)))))
(def-lazy "M-tail" (cons nil (cons nil (cons t (cons nil nil)))))
(def-lazy "N-tail" (cons nil (cons nil (cons nil (cons t nil)))))
(def-lazy "O-tail" (cons nil (cons nil (cons nil (cons nil nil)))))
(def-lazy "P-tail" (cons t (cons t (cons t (cons t nil)))))
(def-lazy "Q-tail" (cons t (cons t (cons t (cons nil nil)))))
(def-lazy "R-tail" (cons t (cons t (cons nil (cons t nil)))))
(def-lazy "S-tail" (cons t (cons t (cons nil (cons nil nil)))))
(def-lazy "T-tail" (cons t (cons nil (cons t (cons t nil)))))
(def-lazy "U-tail" (cons t (cons nil (cons t (cons nil nil)))))
(def-lazy "V-tail" (cons t (cons nil (cons nil (cons t nil)))))
(def-lazy "W-tail" (cons t (cons nil (cons nil (cons nil nil)))))
(def-lazy "X-tail" (cons nil (cons t (cons t (cons t nil)))))
(def-lazy "Y-tail" (cons nil (cons t (cons t (cons nil nil)))))
(def-lazy "Z-tail" (cons nil (cons t (cons nil (cons t nil)))))
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

