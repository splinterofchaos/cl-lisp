
(declfun fib int int)
(defun fib (x) (if (< x 2) (x)
                 (+ (fib (- x 1))
                    (fib (- x 2)))))

(printf "fib(1) = %d\n" (fib 1))
(printf "fib(2) = %d\n" (fib 2))
(printf "fib(3) = %d\n" (fib 3))
(printf "fib(4) = %d\n" (fib 4))
(printf "fib(5) = %d\n" (fib 5))
(printf "fib(6) = %d\n" (fib 6))
(printf "fib(7) = %d\n" (fib 7))
(printf "fib(8) = %d\n" (fib 8))
