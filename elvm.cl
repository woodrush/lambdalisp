(load "./lazy.cl")


(def-lazy " " 32)

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


;; (def-lazy int-zero (take 24 (inflist nil)))
(def-lazy "0" (+ 32 16))

(def-lazy int-zero (list nil nil nil nil nil nil nil nil))


;;================================================================
;; Memory
;;================================================================
(def-lazy init-memory nil)

(defrec-lazy memory-lookup (memory address)
  (cond
    ((isnil memory)
      int-zero)
    ((isnil address)
      memory)
    (t
      (memory-lookup (memory (car address)) (cdr address)))))

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


;;================================================================
;; Registers
;;================================================================
(defun-lazy reg-A  (r0 r1 r2 r3 r4 r5) r0)
(defun-lazy reg-B  (r0 r1 r2 r3 r4 r5) r1)
(defun-lazy reg-C  (r0 r1 r2 r3 r4 r5) r2)
(defun-lazy reg-D  (r0 r1 r2 r3 r4 r5) r3)
(defun-lazy reg-SP (r0 r1 r2 r3 r4 r5) r4)
(defun-lazy reg-BP (r0 r1 r2 r3 r4 r5) r5)
(defun-lazy cons6 (r0 r1 r2 r3 r4 r5 f) (f r0 r1 r2 r3 r4 r5))

(def-lazy init-reg
  (cons6 int-zero int-zero int-zero int-zero int-zero int-zero))

(defmacro-lazy reg-read (reg regptr)
  `(,reg ,regptr))

(defun-lazy reg-write (reg regptr value)
  (regptr
    (cons6  value               (reg-read reg reg-B) (reg-read reg reg-C) (reg-read reg reg-D) (reg-read reg reg-SP) (reg-read reg reg-BP))
    (cons6 (reg-read reg reg-A)  value               (reg-read reg reg-C) (reg-read reg reg-D) (reg-read reg reg-SP) (reg-read reg reg-BP))
    (cons6 (reg-read reg reg-A) (reg-read reg reg-B)  value               (reg-read reg reg-D) (reg-read reg reg-SP) (reg-read reg reg-BP))
    (cons6 (reg-read reg reg-A) (reg-read reg reg-B) (reg-read reg reg-C)  value               (reg-read reg reg-SP) (reg-read reg reg-BP))
    (cons6 (reg-read reg reg-A) (reg-read reg reg-B) (reg-read reg reg-C) (reg-read reg reg-D)  value                (reg-read reg reg-BP))
    (cons6 (reg-read reg reg-A) (reg-read reg reg-B) (reg-read reg reg-C) (reg-read reg reg-D) (reg-read reg reg-SP)  value               )))


;;================================================================
;; Arithmetic
;;================================================================
(defrec-lazy invert (n)
  (if (isnil n)
    nil
    (cons (not (car n)) (invert (cdr n)))))

(defrec-lazy add-carry (n m carry)
  (cond ((isnil n)
          nil)
        (t
          (if (car n)
            (if (car m)
              (cons carry       (add-carry (cdr n) (cdr m) t))
              (cons (not carry) (add-carry (cdr n) (cdr m) carry)))
            (if (car m)
              (cons (not carry) (add-carry (cdr n) (cdr m) carry))
              (cons carry       (add-carry (cdr n) (cdr m) nil)))))))

(defmacro-lazy add (n m)
  `(add-carry ,n ,m nil))

(defmacro-lazy sub (n m)
  `(add-carry ,n (invert ,m) t))


;;================================================================
;; I/O
;;================================================================
(def-lazy powerlist
  (cons 1 (cons 2 (cons 4 (cons 8 (cons 16 (cons 32 (cons 64 (cons 128 nil)))))))))

(def-lazy invpowerlist
  (cons 128 (cons 64 (cons 32 (cons 16 (cons 8 (cons 4 (cons 2 (cons 1 nil)))))))))

(defrec-lazy bit2int* (n powerlist)
  (cond ((isnil powerlist)
          0)
        (t
          (if (car n)
            (+ (car powerlist) (bit2int* (cdr n) (cdr powerlist)))
            (bit2int* (cdr n) (cdr powerlist))))))

(defmacro-lazy bit2int (n)
  `(bit2int* ,n powerlist))

(defrec-lazy int2bit* (n invpowerlist)
  (cond ((isnil invpowerlist)
          nil)
        ((iszero n)
          (cons nil (int2bit* n (cdr invpowerlist))))
        (t
          (if (<= (car invpowerlist) n)
            (cons t   (int2bit* (- n (car invpowerlist)) (cdr invpowerlist)))
            (cons nil (int2bit* n (cdr invpowerlist)))))))

(defmacro-lazy int2bit (n)
  `(reverse (int2bit* ,n invpowerlist)))


;;================================================================
;; Instructions
;;================================================================
(defun-lazy inst-mov-imm (imm *dst reg memory program cont)
  (cont (reg-write reg *dst imm) memory program))

(defun-lazy inst-mov-reg (*src *dst reg memory program cont)
  (cont (reg-write reg *dst (reg-read reg *src)) memory program))


(defun-lazy truth2int (x) (x 1 0))

(defun-lazy main (stdin)
  (do-continuation
    (let* memory init-memory)
    (let* reg init-reg)
    (let* address (list t t nil))
    ;; (let ((value (list "A"))))
    ;; (let ((memory (memory-write memory address value))))
    ;; (let* ret  (memory-lookup memory address))
    ;; (let* ret2 (memory-lookup memory (list t t t)))
    ;; (let* ret3 (memory-lookup memory (list t t nil nil)))
    ;; (let* ret4 (memory-lookup memory (list t t nil nil)))
    ;; (cons (car ret))
    ;; (cons (car ret2))
    ;; (cons (car ret3))
    ;; (cons (car ret4))

    ;; (let* memory (memory-write memory address (int2bit 32)))
    ;; (let* n (memory-lookup memory address))

    ;; (let* reg (reg-write reg reg-B (int2bit 32)))
    ;; (let* n (reg-read reg reg-B))

    ;; (let* n (int2bit 32))

    (<- (reg memory program) (inst-mov-imm (int2bit 32) reg-A reg memory nil))
    (<- (reg memory program) (inst-mov-reg reg-A reg-B reg memory nil))
    (let* n (reg-read reg reg-B))

    (let* m (int2bit 4))
    (let* a (int2bit 2))
    (let* zx (add (add n m) a))
    (let* zy (add (add (add n m) m) a))
    (let* zz (sub (add (add n m) m) a))
    ;; (let ((c (bit2int (list t nil nil t t t nil nil)))))
    ;; (let ((c (bit2int x))))
    ;; ;; (let ((d (bit2int (int2bit (+ 4 32))))))
    ;; (let ((e (bit2int x))))
    (cons (+ "0" (length zx)))
    (cons " ")
    (cons (bit2int n))
    (cons (bit2int zx))
    (cons (bit2int zy))
    (cons (bit2int zz))
    (cons " ")
    (cons (+ "0" (-> zx car truth2int)))
    (cons (+ "0" (-> zx cdr car truth2int)))
    (cons (+ "0" (-> zx cdr cdr car truth2int)))
    (cons (+ "0" (-> zx cdr cdr cdr car truth2int)))
    (cons (+ "0" (-> zx cdr cdr cdr cdr car truth2int)))
    (cons (+ "0" (-> zx cdr cdr cdr cdr cdr car truth2int)))
    (cons (+ "0" (-> zx cdr cdr cdr cdr cdr cdr car truth2int)))
    (cons (+ "0" (-> zx cdr cdr cdr cdr cdr cdr cdr car truth2int)))

    ;; (cons c)
    ;; ;; (cons d)
    ;; (cons e)
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
