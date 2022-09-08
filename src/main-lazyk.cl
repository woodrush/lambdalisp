(load "./lambdalisp.cl")
(load "./lazyk-chars.cl")
(load "./lazyk-ulamb-blc-wrapper.cl")

;; Override the prelude definition with the Lazy K optimized version
(load "./build/def-prelude-lazyk.cl")


(format t (compile-to-ski-lazy (blc-to-lazyk-wrapper main)))
