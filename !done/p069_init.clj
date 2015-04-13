; Q: initを自作せよ。
; A
(defn myinit [xs]
  (if (= 1 (count xs))
    ()
    (cons (first xs) (myinit (rest xs)))))
