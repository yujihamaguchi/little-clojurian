; Q: 関数takeWhileを自作せよ。
; A
(defn mytakeWhile [p xs]
	(if (empty? xs)
		()
		(if (p (first xs))
			(cons (first xs) (mytakeWhile p (rest xs)))
			())))