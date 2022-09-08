(load "./lambdalisp.cl")
(load "./lazyk-ulamb-blc-wrapper.cl")


(format t (compile-to-blc-lazy (blc-to-ulamb-wrapper main)))
