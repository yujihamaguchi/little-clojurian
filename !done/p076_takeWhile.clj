; Q: リストの先頭から述語を満たす連続した要素を取り出す関数takeWhileを自作せよ。
; A:
(defn myTakeWhile [p xs]
	(if (or (empty? xs) (not (p (first xs))))
		()
		(cons (first xs) (myTakeWhile p (rest xs)))))