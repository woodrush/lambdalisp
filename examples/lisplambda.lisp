(setq input "0010")

(defun parsevarname (s n cont)
  (cond
    ((= nil s)
      (error "Parse error: Unexpected EOF in variable name"))
    ((= "0" (carstr s))
      (cont (cdrstr s) n))
    ((= "1" (carstr s))
      (parsevarname (cdrstr s) (+ 1 n) cont))))

(defun parseblc (s)
  (print s)
  (cond
    ((= s nil)
      nil)
    ((= (carstr s) "0")
      (cond
        ((= nil (cdrstr s))
          (error "Parse error: Unexpected EOF"))
        ((= (carstr (cdrstr s) "0"))
          (cons (quote L) (parseblc (cdrstr (cdrstr s)))))
        ;; case 1
        (t
          (cons (quote P) (parseblc (cdrstr (cdrstr s)))))))
    ;; case 1
    (t
      (parsevarname (cdrstr s) 0
        (lambda (s n)
          (cons n (parseblc s)))))))

(print (parseblc input))
(print "End")
