; Q: リストの要素のどれかが述語を満たすか検査する関数anyを自作せよ。
; A:
(defn myany [p xs]
	(if (empty? xs)
		false
		(or (p (first xs)) (myany p (rest xs)))))