(load "./lazy.cl")

(defun-lazy checkbit (m target)
  ((lambda (mdash)
       (iszero mdash
         (cons t target)
         (cons nil (pred mdash))))
     (m pred (succ target))))

(def-lazy int2bitlist-helper
  ((lambda (x) (x x))
   (lambda (self numlist target)
     (if (isnil numlist)
         nil
         ((lambda (tuple)
            (cons (car tuple) (self self (cdr numlist) (cdr tuple))))
          (checkbit (car numlist) target))))))

(def-lazy powerlist
  (cons 128 (cons 64 (cons 32 (cons 16 (cons 8 (cons 4 (cons 2 (cons 1 nil)))))))))

(defun-lazy int2bitlist (target)
  (int2bitlist-helper powerlist target))

(def-lazy lazystr2blcstr
  ((lambda (x) (x x))
   (lambda (self lazystr)
     (iszero (256 pred (succ (car lazystr)))
         (cons (int2bitlist (car lazystr))
               (self self (cdr lazystr)))
         nil))))

(def-lazy blcchar2lazychar-helper
  ((lambda (x) (x x))
   (lambda (self ret numlist blcchar)
     (if (isnil numlist)
         ret
         (self self (if (car blcchar)
                        ret
                        (+ (car numlist) ret))
                    (cdr numlist)
                    (cdr blcchar))))))

(defun-lazy blcchar2lazychar (blcchar)
  (blcchar2lazychar-helper 0 powerlist blcchar))

(def-lazy blcstr2lazystr
  ((lambda (x) (x x))
   (lambda (self blcstr)
     (if (isnil blcstr)
         (inflist 256)
         (cons (blcchar2lazychar (car blcstr))
               (self self (cdr blcstr)))))))

(defun-lazy main (stdin)
  (lazystr2blcstr (program (blcstr2lazystr stdin))))

(print (compile-to-blc-lazy main))
