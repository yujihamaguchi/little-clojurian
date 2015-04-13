; Q: 目的とする値がリストのどの位置にあるかを調べて、その位置全てをリストとして返す関数positionsを用いて書け。
; A
(defn positions [key coll]
  (for [[idx elt] (map vector (iterate inc 0) coll) :when (= key elt)] idx))