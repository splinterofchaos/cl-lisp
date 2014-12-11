(defun fact (x)
  (if (> x 1) (* x (fact (- x 1))) (x)))


(printf "fact(1) = %d\n" (fact 1))
(printf "fact(2) = %.0f\n" (fact 2.0))
(printf "fact(3) = %d\n" (fact 3))
(printf "fact(4) = %.0f\n" (fact 4.0))
(printf "fact(5) = %d\n" (fact 5))
(printf "fact(6) = %d\n" (fact 6))
(printf "fact(7) = %d\n" (fact 7))
(printf "fact(8) = %d\n" (fact 8))
