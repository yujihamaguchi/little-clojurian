; Q: ビットのリストで表現される二進表記を整数に変換する関数bit2intを書け。
;    ・iterateを用いること
;    ・二進表記は逆順であること
; type Bit = Int
; bit2int :: [Bit] -> Int
; bit2int bits = sum [b * w | (b, w) <- zip bits weights]
;                 where
;                   weights = iterate (*2) 1
(defn bit2int [bs]
	(reduce + (for [[b w] (map vector bs (iterate #(* % 2) 1))] (* (Integer/parseInt (str b)) w))))