; Q: 空でないリストの最後の要素を取得する関数mylastを書け
; A
(defn mylast [xs]
	(if (empty? (rest xs))
		(first xs)
		(mylast (rest xs))))
