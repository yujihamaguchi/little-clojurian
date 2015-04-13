; Q: ある要素のみからなるリストを生成する関数myreplicateを書け。
;    ex) >replicate 3 True
;        [True, True, True]
; (defn myreplicate [n x]
; 	(if (zero? n)
; 		()
; 		(cons x (myreplicate (dec n) x))))
(defn myreplicate [n x]
	(for [n' (range 1 (inc n))] x)) 