(defun plus-one (x) (+ x 1))
(printf "5 + 1 + 1 = %i\n" (progn
                             (setq x (plus-one 5))
                             (plus-one x)))
(progn
  (setq x 1)
  (printf "1 + 1 = %i\n" (plus-one x))
  (printf "1 = %i\n" x))

(defun hello (str) (printf str))
(hello "hello ")
(setq str "world\n")
(hello str)
