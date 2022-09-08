(if (not (boundp 'lambdacraft-loaded))
  (load "./lambdacraft.cl"))


(def-lazy powerlist
  (cons 128 (cons 64 (cons 32 (cons 16 (cons 8 (cons 4 (cons 2 (cons 1 nil)))))))))

(defrec-lazy int2bitlist (n powerlist cont)
  (if (isnil powerlist)
    (cont nil)
    (do
      (<- (car-pow cdr-pow) (powerlist))
      (if-then-return (<= car-pow n)
        (do
          (<- (nextlist) (int2bitlist (- n car-pow) cdr-pow))
          (cont (cons nil nextlist))))
      (<- (nextlist) (int2bitlist n cdr-pow))
      (cont (cons t nextlist)))))

(defrec-lazy ulambstr-to-blcstr (s)
  (cond
    ((isnil s)
      nil)
    (t
      (do
        (<- (c-ulamb s-cdr) (s))
        (<- (c-blc) (int2bitlist c-ulamb powerlist))
        (cons c-blc (ulambstr-to-blcstr s-cdr))))))

(defrec-lazy bitlist2int (n powerlist cont)
  (if (isnil powerlist)
    (cont 0)
    (do
      (<- (car-pow cdr-pow) (powerlist))
      (<- (car-n cdr-n) (n))
      (<- (n-ret) (bitlist2int cdr-n cdr-pow))
      (if car-n
        (cont n-ret)
        (cont (+ car-pow n-ret))))))

(defun-lazy blcchar-to-ulambchar (c cont)
  (cond
    ((isnil c)
      (cont nil))
    (t
      (bitlist2int c powerlist cont))))

(defun-lazy blcchar-to-lazychar (c cont)
  (cond
    ((isnil c)
      (cont nil))
    (t
      (bitlist2int c powerlist cont))))


(defrec-lazy blcstr-to-ulambstr (s)
  (cond
    ((isnil s)
      nil)
    (t
      (do
        (<- (c-blc s-cdr) (s))
        (<- (c-ulamb) (blcchar-to-ulambchar c-blc))
        (cons c-ulamb (blcstr-to-ulambstr s-cdr))))))


;; Lazy K
(defrec-lazy blcstr-to-lazykstr (s)
  (cond
    ((isnil s)
      (inflist 256))
    (t
      (do
        (<- (c-blc s-cdr) (s))
        (<- (c-lazy) (blcchar-to-lazychar c-blc))
        (cons c-lazy (blcstr-to-lazykstr s-cdr))))))

(defrec-lazy lazykstr-to-blcstr (s)
  (do
    (<- (c-ulamb s-cdr) (s))
    (if-then-return (= 256 c-ulamb)
      nil)
    (<- (c-blc) (int2bitlist c-ulamb powerlist))
    (cons c-blc (lazykstr-to-blcstr s-cdr))))


(defun-lazy blc-to-lazyk-wrapper (program stdin)
  (blcstr-to-lazykstr (program (lazykstr-to-blcstr stdin))))

(defun-lazy blc-to-ulamb-wrapper (program stdin)
  (blcstr-to-ulambstr (program (ulambstr-to-blcstr stdin))))
