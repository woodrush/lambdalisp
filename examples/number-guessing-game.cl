(defparameter **lambdalisp-suppress-repl** t) ;; Enters script mode and suppresses REPL messages

(defun number-guessing-game ()
  (let ((n ()) (n-tries 0) (answer (* 3 14)))
    (loop
      (format t "Let's play a number guessing game. I'm thinking of a number between 1 and 10000.~%")
      (format t "Say a number, and I'll tell you if it's less than, greater than, or equal to my number.~%")
      (format t "Can you guess what number I'm thinking of?~%")
      (format t "Please enter a number:~%")
      (block gameloop
        (loop
          (block input-n
            (loop
              (format t "number> ")
              (finish-output nil)
              (setq n (read))
              (if (integerp n)
                (return-from input-n))
              (format t "Please enter a number (you entered ~a).~%" n)))

          (setq n-tries (+ 1 n-tries))
          (cond
            ((< n answer)
              (format t "Your guess is less than my number.~%"))
            ((> n answer)
              (format t "Your guess is greater than my number.~%"))
            (t
              (return-from gameloop)))))
      (format t "That's right! I was thinking of ~a. Congratulations!~%" n)
      (format t "Number of tries: ~a~%" n-tries)
      (format t "Do you want to try again? [y/N]~%")
      (format t "y/N> ")
      (finish-output nil)
      (setq n (read))
      (cond
        ((eq n 'y)
          (format t "All right!~%")
          (setq n-tries 0))
        (t
          (return))))
    (format t "Thank you for playing!~%")
    (exit)))

(number-guessing-game)
