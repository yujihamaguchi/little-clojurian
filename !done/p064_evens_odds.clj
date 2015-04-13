; Q: リストから偶数の位置の要素を取り出す関数evensと、奇数の位置の要素を取り出す関数oddsを相互再帰を用いて書け。
; A
(declare evens odds)
(defn evens [xs]
  (if (empty? xs)
    ()
    (odds (rest xs))))
(defn odds [xs]
  (if (empty? xs)
    ()
    (cons (first xs) (evens (rest xs)))))