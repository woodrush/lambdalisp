(load "./lazy.cl")


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
        (t
          `(do-continuation*
              ,(append (car proc) `(,top))
              ,@(cdr proc)))))

(defmacro-lazy do-continuation (&rest proc)
  `(do-continuation* ,@(reverse proc)))


;; (def-lazy zero-int (take 24 (inflist nil)))
(def-lazy "0" (+ 32 16))

(def-lazy zero-int (list "0"))
(def-lazy init-memory nil)

(defrec-lazy memory-lookup-cont (memory address cont)
  (cond
    ((isnil address)
      (if (isnil memory)
        (cont zero-int)
        (cont memory)))
    ((isnil memory)
      (cont zero-int))
    (t
      (memory-lookup-cont (memory (car address)) (cdr address) cont))))

(defrec-lazy memory-write (memory address value)
  (cond
    ((isnil address)
      value)
    ((isnil memory)
      ((car address)
        (cons (memory-write nil (cdr address) value) nil)
        (cons nil (memory-write nil (cdr address) value))))
    (t
      ((car address)
        (cons (memory-write (car memory) (cdr address) value) (cdr memory))
        (cons (car memory) (memory-write (cdr memory) (cdr address) value))))))

(defun-lazy enum-A  (r0 r1 r2 r3 r4 r5) r0)
(defun-lazy enum-B  (r0 r1 r2 r3 r4 r5) r1)
(defun-lazy enum-C  (r0 r1 r2 r3 r4 r5) r2)
(defun-lazy enum-D  (r0 r1 r2 r3 r4 r5) r3)
(defun-lazy enum-SP (r0 r1 r2 r3 r4 r5) r4)
(defun-lazy enum-BP (r0 r1 r2 r3 r4 r5) r5)
(defun-lazy cons6 (r0 r1 r2 r3 r4 r5 f) (f r0 r1 r2 r3 r4 r5))

(def-lazy *A  0)
(def-lazy *B  1)
(def-lazy *C  2)
(def-lazy *D  3)
(def-lazy *SP 4)
(def-lazy *BP 5)

(defmacro-lazy regread (reg regptr)
  `(,reg ,regptr))

(defun-lazy reg-write (reg regptr value)
  (regptr
    (cons6  value               (regread reg enum-B) (regread reg enum-C) (regread reg enum-D) (regread reg enum-SP) (regread reg enum-BP))
    (cons6 (regread reg enum-A)  value               (regread reg enum-C) (regread reg enum-D) (regread reg enum-SP) (regread reg enum-BP))
    (cons6 (regread reg enum-A) (regread reg enum-B)  value               (regread reg enum-D) (regread reg enum-SP) (regread reg enum-BP))
    (cons6 (regread reg enum-A) (regread reg enum-B) (regread reg enum-C)  value               (regread reg enum-SP) (regread reg enum-BP))
    (cons6 (regread reg enum-A) (regread reg enum-B) (regread reg enum-C) (regread reg enum-D)  value                (regread reg enum-BP))
    (cons6 (regread reg enum-A) (regread reg enum-B) (regread reg enum-C) (regread reg enum-D) (regread reg enum-SP)  value               )))

(defrec-lazy invert (n)
  (cons (not (car n)) (invert (cdr n))))

(defrec-lazy add-carry (n m carry)
  (cond ((isnil n)
          nil)
        (t
          (if (car n)
            (if (car m)
              (cons carry       (add (cdr n) (cdr m) t))
              (cons (not carry) (add (cdr n) (cdr m) carry)))
            (if (car m)
              (cons (not carry) (add (cdr n) (cdr m) carry))
              (cons carry       (add (cdr n) (cdr m) (not carry))))))))

(defmacro-lazy add (n m)
  `(add-carry ,n ,m nil))

(defmacro-lazy sub (n m)
  `(add-carry ,n (invert ,m) t))


(defun-lazy mov-imm (src dst))
(defun-lazy mov-reg (src dst))

(defun-lazy halfadd (x y)
  (if x
    (if y
      (cons ))))

(defun-lazy main (stdin)
  (do-continuation
    (let ((memory init-memory)))
    (let ((address (list t t nil))))
    (let ((value (list "A"))))
    (let ((memory (memory-write memory address value))))
    (<- (ret) (memory-lookup-cont memory address))
    ;; (let ((ret2 "B")))
    (<- (ret2) (memory-lookup-cont memory (list t t t)))
    (<- (ret3) (memory-lookup-cont memory (list t t nil nil)))
    (let ((memory (memory-write memory (list t t nil nil) value))))
    (<- (ret4) (memory-lookup-cont memory (list t t nil nil)))
    (cons (car ret))
    (cons (car ret2))
    (cons (car ret3))
    (cons (car ret4))
    (inflist 256)))



;;================================================================
;; Code output
;;================================================================
(format t (compile-to-ski-lazy main))
;; (format t (compile-to-blc-lazy main))

;; ;; Print lambda term
;; (setf *print-right-margin* 800)
;; (format t (write-to-string (curry (macroexpand-lazy main))))

;; ;; Print in curried De Bruijn notation
;; (format t (write-to-string (to-de-bruijn (curry (macroexpand-lazy main)))))
