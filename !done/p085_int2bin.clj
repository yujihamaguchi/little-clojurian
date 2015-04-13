; Q: 負でない整数を二進表記へ変換する関数int2binを書け。
; A
(defn int2bit [n]
	(if (zero? n)
		()
		(cons (mod n 2) (int2bit (quot n 2)))))