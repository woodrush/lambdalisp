(load "./lazy.cl")


(def-lazy "(" (+ 32 8))
(def-lazy ")" (succ "("))
(def-lazy " " 32)
(def-lazy "\\n" (+ 8 2))

(def-lazy "A" (succ 64))
(defmacro def-alphabet-lazy ()
  (let* ((alphabet (coerce "ABCDEFGHIJKLMNOPQRSTUVWXYZ" `list))
        (alphazip (mapcar #'list (cdr alphabet) alphabet))
        (expr (map 'list #'(lambda (z) `(def-lazy ,(string (car z)) (succ ,(string (car (cdr z)))))) alphazip)))
    `(progn ,@expr)))
(def-alphabet-lazy)

(def-lazy "a" (succ (+ 64 32)))
(defmacro def-alphabet-lazy ()
  (let* ((alphabet (coerce "abcdefghijklmnopqrstuvwxyz" `list))
        (alphazip (mapcar #'list (cdr alphabet) alphabet))
        (expr (map 'list #'(lambda (z) `(def-lazy ,(string (car z)) (succ ,(string (car (cdr z)))))) alphazip)))
    `(progn ,@expr)))
(def-alphabet-lazy)


;; (def-lazy "B" (succ "A"))
;; (def-lazy "C" (succ "B"))
;; (def-lazy "D" (succ "C"))


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

(def-lazy typeof car)
(def-lazy valueof cdr)

(defrec-lazy printexpr (atomenv expr)
  (typematch (typeof expr)
    ;; atom
    (str2stream (car ((valueof expr) cdr atomenv)))
    ;; list
    (catstreamlist
      (list (char2stream "(")
            ((letrec-lazy interleave-space (slist)
                (if (isnil (cdr slist))
                  (car slist)
                  (catstreamlist (list (car slist) (char2stream " ") (interleave-space (cdr slist))))))
              (map (printexpr atomenv) (valueof expr)))
            (char2stream ")"))))
  ;; (let ((exprtype (typeof expr)) ((valueof expr) (cdr expr)))
  ;;   )
    )


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
            (cons (cons type-list curlist) (cons atomenv (cdr stdin))))
          ((= "(" c)
            (let ((readoutstate (read-list nil (cdr stdin) atomenv))
                  (readoutlist (car readoutstate))
                  (atomenv (car (cdr readoutstate)))
                  (stdin (cdr (cdr readoutstate))))
              (read-list (append-element curlist readoutlist) stdin atomenv)))
          (t
            (let ((readoutstate (read-atom nullstream stdin))
                  (readoutstream (car readoutstate))
                  (stdin (cdr readoutstate))
                  (ret-atomlookup (get-atomindex-env atomenv (readoutstream nil)))
                  (ret-atom (atom* (car ret-atomlookup)))
                  (ret-atomenv (cdr ret-atomlookup)))
              ;; (cons (atom* 1) nil)
              (read-list (append-element curlist 
              ;; (atom* 0)
              ret-atom
              )
              stdin
              ret-atomenv)
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


(def-lazy initialenv
  (list
    (list "q" "u" "o" "t" "e")
    (list "c" "a" "r")
    (list "c" "d" "r")
    (list "c" "o" "n" "s")
    (list "a" "t" "o" "m")
    (list "e" "q")
    (list "c" "o" "n" "d")
    (list "p" "r" "i" "n" "t")
    (list "r" "e" "a" "d")
    (list "t")))

(defrec-lazy eval (expr varenv atomenv stdin stdoutstream)
  (typematch (typeof expr)
    ;; atom
    nil
    ;; list
    (let ((head (car (valueof expr)))
          (hvalue (valueof head))
          (etail (car (cdr (valueof expr)))))
      (typematch (typeof head)
        ;; atom
        (cond
          ;; quote
          ((= hvalue 0)
            etail)
          ;; car
          ((= hvalue 1)
            (car (valueof (eval etail varenv atomenv stdin stdoutstream))))
          ;; cdr
          ((= hvalue 2)
            (cons type-list (cdr (valueof (eval etail varenv atomenv stdin stdoutstream)))))
          (t
            nil)
          )
        ;; list
        nil
        ))
      ))

(defun-lazy main (stdin)
  ;; (if (stringeq (list "A" "A") (list "A" "A"))
  ;;   (list "A" "A" 256 256)
  ;;   (list "A" "B" 256 256))

;; (list (list "A") (list "B") (list "A" "B" "C") (list "C"))
  (let ((env initialenv)
        (ret-parse (read-list nil stdin env))
        (env (car (cdr ret-parse)))
        (expr (car ret-parse)))
    ((printexpr
        env
        (eval expr nil env stdin nil)
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
