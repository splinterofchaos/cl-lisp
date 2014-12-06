(if 1 (printf "good\n") 0)
(if 0 0 (if 0 0 (printf "good\n")))
(printf "1 = %i\n" (if T T NIL))
(printf "2 = %i\n" (if 1
                     (progn (setq x 2))
                     (progn (setq x 1))))
