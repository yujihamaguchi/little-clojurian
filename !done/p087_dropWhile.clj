; Q: 関数dropWhileを自作せよ。
; A
(defn mydropWhile [p xs]
	(if (empty? xs)
		()
		(if (p (first xs))
			(mydropWhile p (rest xs))
			xs))) 