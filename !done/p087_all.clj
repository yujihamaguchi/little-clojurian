; Q: 関数allを自作せよ。
; A
(defn myall [p xs]
	(if (empty? xs)
		true
		(and (p (first xs)) (myall p (rest xs)))))