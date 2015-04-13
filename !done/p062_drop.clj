; Q: dropを再帰を用いて自作せよ。
; A
(defn mydrop [n xs]
	(if (zero? n)
		xs
		(mydrop (- n 1) (rest xs))))
