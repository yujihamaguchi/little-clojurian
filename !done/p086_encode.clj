; Q: 文字列をビット列に符号化する関数encodeを書け。
;    それぞれの文字列をunicodeのコードポイント（整数）に変換し、さらに8ビットの二進表記に直して、全体を連結することで、ビットのリストを作る。高階関数mapと関数合成を用いて実装せよ。
; A
(defn int2bit [n]
	(if (zero? n)
		()
		(cons (mod n 2) (int2bit (quot n 2)))))
(defn make8 [bs]
	(take 8 (concat bs (replicate 7 0))))
(defn encode [str]
	(apply concat (map (comp make8 int2bit int) str)))
