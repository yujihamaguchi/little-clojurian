; --Q: 整列された要素を持つリストに要素を挿入する関数insertを書け。
; --A
(defn myinsert [x xs]
	(if (empty? xs)
		()
		(if (<= x (first xs))
			(cons x xs)
			(cons (first xs) (myinsert x (rest xs))))))