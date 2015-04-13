; Q: !!の前置記法版のindex関数を再帰を用いて自作せよ。
; A
(defn index [xs n]
  (if (zero? n)
    (first xs)
    (index (rest xs) (dec n))))
