(load "./lazy.cl")


(def-lazy "(" (+ 32 8))
(def-lazy ")" (succ "("))
(def-lazy " " 32)
(def-lazy "\\n" (+ 8 2))

(def-lazy "A" (succ 64))
(def-lazy "B" (succ "A"))
(def-lazy "C" (succ "B"))
(def-lazy "D" (succ "C"))


(defrec-lazy map (f list)
  (if (isnil list)
    nil
    (cons (f (car list)) (map f (cdr list)))))

(defrec-lazy append-element (l item)
  (if (isnil l) (cons item nil) (cons (car l) (append-element (cdr l) item))))

(defrec-lazy append-list (l item)
  (if (isnil l) item (cons (car l) (append-list (cdr l) item))))


(defmacro-lazy typematch (tvar t0 t1)
  `(,tvar ,t0 ,t1))

(defun-lazy type-atom (t0 t1) t0)
(defun-lazy type-list (t0 t1) t1)

(defun-lazy char2stream (char)
  (cons char))

(defun-lazy catstream (stream1 stream2)
  (lambda (stream) (stream1 (stream2 stream))))

(def-lazy nullstream (lambda (x) x))

(defrec-lazy catstreamlist (streamlist)
  (if (isnil streamlist)
    nullstream
    (catstream (car streamlist) (catstreamlist (cdr streamlist)))))

(defrec-lazy str2stream (s)
  (catstreamlist (map char2stream s)))

(defrec-lazy printexpr (atomenv expr)
  (let ((exprtype (car expr)) (value (cdr expr)))
    (typematch exprtype
      ;; atom
      (str2stream (car (value cdr atomenv)))
      ;; list
      (catstreamlist
        (list (char2stream "(")
              ((letrec-lazy interleave-space (slist)
                  (if (isnil (cdr slist))
                    (car slist)
                    (catstreamlist (list (car slist) (char2stream " ") (interleave-space (cdr slist))))))
               (map (printexpr atomenv) value))
              (char2stream ")"))))))


(defrec-lazy read-atom (curstream stdin)
  (let ((c (car stdin)))
    (cond
          ;; ((or (= " " c) (= "\\n" c))
          ;;   (cons curstream (cdr stdin)))
          ((or (= "(" c) (= ")" c) (= 256 c) (= " " c) (= "\\n" c))
            (cons curstream stdin))
          (t
            (read-atom (catstream curstream (char2stream (car stdin))) (cdr stdin))))))

(defrec-lazy stringeq (s1 s2)
  (cond ((and (isnil s1) (isnil s2))
          t)
        ((or  (and (not (isnil s1)) (isnil s2))
              (and (isnil s1) (not (isnil s2))))
          nil)
        ((= (car s1) (car s2))
          (stringeq (cdr s1) (cdr s2)))
        (t
          nil)))

(defun-lazy get-atomindex-env (atomenv str)
  ((letrec-lazy get-atomindex-env-helper (cur-atomenv n)
    (cond ((isnil cur-atomenv)
            (cons n (append-element atomenv str)))
          ((stringeq (car cur-atomenv) str)
            (cons n atomenv))
          (t
            (get-atomindex-env-helper (cdr cur-atomenv) (succ n))))
            )
   atomenv 0))

;; untested
(defrec-lazy read-skip-until-newline (stdin)
  (let ((c (car stdin)))
    (if (= "\\n" c)
      (cdr stdin)
      (read-skip-until-newline (cdr stdin)))))

(defrec-lazy read-skip-whitespace (stdin)
  (let ((c (car stdin)))
    (cond ((or (= " " c) (= "\\n" c))
            (read-skip-whitespace (cdr stdin)))
          (t
            stdin))))


(defrec-lazy read-list (curlist stdin atomenv)
  (let ((stdin (read-skip-whitespace stdin)) (c (car stdin)))
    (cond ((or (= ")" c) (= 256 c))
            (cons (cons type-list curlist) (cdr stdin)))
          ((= "(" c)
            (let ((readoutstate (read-list nil (cdr stdin) atomenv))
                  (readoutlist (car readoutstate))
                  (stdin (cdr readoutstate)))
              (read-list (append-element curlist readoutlist) stdin atomenv)))
          (t
            (let ((readoutstate (read-atom nullstream stdin))
                  (readoutstream (car readoutstate))
                  (stdin (cdr readoutstate))
                  (ret-atomlookup (get-atomindex-env atomenv (readoutstream nil)))
                  (ret-atom (atom* (car ret-atomlookup))))
              ;; (cons (atom* 1) nil)
              (read-list (append-element curlist 
              ;; (atom* 0)
              ret-atom
              )
              stdin
              atomenv)
              )
              )
    ))
    )

;; (defrec-lazy parseexpr (self stream)
;;   (let ((c (car stream)))
;;     (cond
;;       ((= c "(")
;;         (cons type-list (self self )))
;;       ((= c ")"))
;;       ((= c " "))
;;       ((= c "\n")))))

(def-lazy test
  (list type-atom))

(defrec-lazy f (x)
  (cons "A" (f nil))
  ;; (lambda (x) (cons "A" (x nil)))
  )

(defmacro-lazy atom* (value)
  `(cons type-atom ,value))

(defmacro-lazy list* (arg &rest args)
  `(cons type-list (list ,arg ,@args)))


(defun-lazy main (stdin)
  ;; (if (stringeq (list "A" "A") (list "A" "A"))
  ;;   (list "A" "A" 256 256)
  ;;   (list "A" "B" 256 256))

  (let ((env (list (list "A") (list "B") (list "A" "B" "C") (list "C"))))
    ((printexpr
        env
        ;; (atom* 1)
        ;; (atom* (car (get-atomindex-env env (list "A"))))
        (car (read-list nil stdin env))
        )
     (inflist 256))
    )

  ;; (cdr (read-atom nullstream stdin))

  ;; ((printexpr
  ;;     (list (char2stream "A") (str2stream (list "A" "B" "C")) (char2stream "C"))
  ;;     (list*  (atom* 2)
  ;;             (list* (atom* 1) (atom* 2) (atom* 0))
  ;;             (atom* 0))
  ;;       )
  ;;  (inflist 256))
  )

;; (print (macroexpand-lazy main))
(format t (compile-to-ski-lazy main))

;; (defun-lazy main (stdin)
;;   (lazystr2blcstr (program (blcstr2lazystr stdin))))

;; (print (compile-to-blc-lazy main))
