(if 1 (printf "good\n") 0)
(if 0 0 (if 0 0 (printf "good\n")))
(printf "2 = %i" (if 1 2 3))
