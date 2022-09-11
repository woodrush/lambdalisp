(load "./lambdalisp.cl")
(load "./lazyk-ulamb-blc-wrapper.cl")

(defun-lazy main* (stdin*)
  ((blc-to-ulamb-wrapper main) stdin*))

(format t (compile-to-plaintext-lambda-lazy main*))

