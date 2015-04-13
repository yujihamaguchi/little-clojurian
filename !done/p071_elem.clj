; Q: elemを再帰を用いて自作せよ。（二項演算子"||"を用いよ）
; A
(defn myelem [x xs]
  (if (empty? xs)
    false
    (or (= x (first xs)) (myelem x (rest xs)))))