(load "./lazy.cl")

(defparameter simple-lambda-env-vars (concatenate 'list (coerce "xyzabcdefghijklmnopqrstuvw" `list) "α" "β" "γ" "δ" "ε" "ζ" "η" "θ"
;; "ι"
"κ"
"μ" "ν" "ξ" "ο" "π" "ρ"
;; "σ"
"τ" "υ" "φ" "χ" "ψ" "ω" (coerce "ABCDEFGHIJKLMNOPQRSTUVWXYZ" `list)))

(defun to-simple-lambda (body &optional (env ()))
  (labels
    ((lookup (env var)
       (let ((i (position var (reverse env) :test #'equal)))
         (if i
          (nth i simple-lambda-env-vars)
          (decorate-varname var)))
         ))
    (if (atom body)
        (lookup env body)
        (if (not (islambda body))
          (format nil "(~a ~a)"
              (to-simple-lambda (car body) env)
              (to-simple-lambda (car (cdr body)) env))
          (format nil "λ~a.~a"
            (lookup (cons (lambdaarg-top body) env) (lambdaarg-top body))
            (to-simple-lambda (lambdabody body) (cons (lambdaarg-top body) env)))))))

(defun count-occurrences-in (expr var)
  (cond ((atom expr) (if (equal var expr) 1 0))
        ((islambda expr)
         (if (equal (lambdaarg-top expr) var)
             0
             (count-occurrences-in (cdr (cdr expr)) var)))
        (t (reduce '+ (mapcar (lambda (x) (count-occurrences-in x var)) expr))
        )))

(defun occurs-freely-in (expr var)
  (cond ((atom expr) (equal var expr))
        ((islambda expr)
         (if (equal (lambdaarg-top expr) var)
             nil
             (occurs-freely-in (cdr (cdr expr)) var)))
        (t (or (occurs-freely-in (car expr) var)
               (occurs-freely-in (cdr expr) var)))))

(defun t-rewrite (expr)
  (cond ((atom expr) expr)
        ((eq 'lambda (car expr))
         (let ((arg  (lambdaarg-top expr))
               (body (lambdabody expr)))
              (cond ((equal arg body) 'I)
                    ((not (occurs-freely-in body arg))
                       `(K ,(t-rewrite body)))
                    ((islambda body)
                       (t-rewrite `(lambda (,arg) ,(t-rewrite body))))
                    (t `((S ,(t-rewrite `(lambda (,arg) ,(car body))))
                            ,(t-rewrite `(lambda (,arg) ,(car (cdr body)))))))))
        (t (mapcar #'t-rewrite expr))))

(defun flatten-ski (expr)
  (if (atom expr)
      (if (position expr `(S K I))
          (string-downcase (string expr))
          (decorate-varname expr))
      (concatenate `string "`" (flatten-ski (car expr)) (flatten-ski (car (cdr expr))))))

(defun compile-to-ski (expr)
  (flatten-ski (t-rewrite (curry expr))))

(defmacro compile-to-ski-lazy (expr-lazy)
  `(compile-to-ski (macroexpand-lazy ,expr-lazy)))

(defun compile-to-simple-lambda (expr)
  (to-simple-lambda (curry expr)))

(defmacro compile-to-simple-lambda-lazy (expr-lazy)
  `(compile-to-simple-lambda (macroexpand-lazy ,expr-lazy)))

;; (def-lazy "A" (succ 64))
;; (defmacro def-alphabet-lazy ()
;;   (let* ((alphabet (coerce "ABCDEFGHIJKLMNOPQRSTUVWXYZ" `list))
;;         (alphazip (mapcar #'list (cdr alphabet) alphabet))
;;         (expr (map 'list #'(lambda (z) `(def-lazy ,(string (car z)) (succ ,(string (car (cdr z)))))) alphazip)))
;;     `(progn ,@expr)))
;; (def-alphabet-lazy)
;; ;; (def-lazy "B" (succ "A"))

;; (def-lazy "a" (succ (+ 64 32)))
;; (defmacro def-alphabet-lazy ()
;;   (let* ((alphabet (coerce "abcdefghijklmnopqrstuvwxyz" `list))
;;         (alphazip (mapcar #'list (cdr alphabet) alphabet))
;;         (expr (map 'list #'(lambda (z) `(def-lazy ,(string (car z)) (succ ,(string (car (cdr z)))))) alphazip)))
;;     `(progn ,@expr)))
;; (def-alphabet-lazy)
