; Q: 数値のリストに対し要素の積を計算する関数myproductを書け
; A
(defn myproduct [ns]
	(if (empty? ns)
		1
		(* (first ns) (myproduct (rest ns)))))
