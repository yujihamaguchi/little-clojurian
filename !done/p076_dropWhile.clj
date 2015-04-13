; Q: リストの先頭から述語を満たす連続した要素を取り除く関数dropWhileを自作せよ。
; A:
(defn myDropWhile [p xs]
	(if (empty? xs)
		()
		(if (p (first xs))
			(myDropWhile p (rest xs))
			xs)))