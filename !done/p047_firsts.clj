; Q: 組のリストから、組の先頭の要素を取り出してリストを生成するfirstsを書け。
; A
(defn firsts [ts]
	(for [[x _] ts] x))
