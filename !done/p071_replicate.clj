; Q: replicateを再帰を用いて自作せよ。
; A
(defn myreplicate [n x]
  (if (zero? n)
    ()
    (cons x (myreplicate (dec n) x))))