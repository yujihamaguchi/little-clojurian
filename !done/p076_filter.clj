; Q: filterを再帰を用いて自作せよ。
; A
(defn myFilter [p xs]
	(if (empty? xs)
		()
		(if (p (first xs))
			(cons (first xs) (myFilter p (rest xs)))
			(myFilter p (rest xs)))))