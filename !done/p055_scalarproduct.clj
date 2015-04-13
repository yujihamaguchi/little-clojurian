; Q: 二つの整数のリストの内積を求める関数scalarproductをリスト内包表記を用いて書け。
; A
(defn scalarproduct [ns ms]
	(reduce + (for [[n, m] (map vector ns ms)] (* n m))))