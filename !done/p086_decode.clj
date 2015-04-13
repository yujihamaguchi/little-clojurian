; Q: ビットのリストを文字列に複合する関数decodeを書け。
;    リストを分割し、二進表記をUnicodeのコードポイント（整数）へ変換し、文字へ直して、全体として文字列にする。
;    関数合成を用いて実装せよ。
; A
(defn chop8 [bs]
	(if (empty? bs)
		()
		(cons (take 8 bs) (chop8 (drop 8 bs)))))
(defn bit2int [bs]
	(reduce + (for [[b, w] (map vector bs (iterate #(* 2 %) 1))] (* b w))))
(defn decode [bs]
	(apply str (map (comp char bit2int) (chop8 bs))))