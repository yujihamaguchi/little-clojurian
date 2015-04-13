; Q: 順序クラスに属する任意の型の要素を持つリストが、整列されているか調べる関数sortedをpairs関数を用いて書け。
; A
(use '[useful.seq :only (zip)])
(defn pairs [xs]
  (zip xs (rest xs)))
(defn sorted? [xs]
  (every? identity (for [[x y] (pairs xs) :when (not (nil? y))] (<= x y))))
; pairsのような処理を行いたい場合、Clojureではシーケンスライブラリのpartitionを使用する。
; (defn sorted? [xs]
; 	(every? identity (for [[x y] (partition 2 1 xs)] (<= x y))))
