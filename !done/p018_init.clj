; Q: 空でないリストの最後の要素を取り除く関数myinitを書け
; A
(defn myinit [xs]
	(if (empty? (rest xs))
		()
		(cons (first xs) (myinit (rest xs)))))
