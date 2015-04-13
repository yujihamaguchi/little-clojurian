; Q: ビット列を8ビットの二進表記に分割する関数chop8を書け。
; A
(defn chop8 [bs]
	(if (empty? bs)
		()
		(lazy-seq (cons (take 8 bs) (chop8 (drop 8 bs))))))