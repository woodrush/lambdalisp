;; Read user input
(printq Please input a number: \n)
(printq number>)
(setq input (read))
(cond
    ((not (isint input))
        (printq The input (unquote input) was not a number. \n))
    ((= (% input 2) 0)
        (printq The input (unquote input) was an even number. \n))
    (else
        (printq The input (unquote input) was an odd number. \n)))
