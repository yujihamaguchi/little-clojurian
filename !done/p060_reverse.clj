; Q: 要素を逆転する関数myreverseを書け。
; A
(defn myreverse [xs]
	(if (empty? xs)
		[]
		(conj (myreverse (vec (rest xs))) (first xs))))
