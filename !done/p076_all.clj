; Q: リストの要素全てが述語を満たすか検査するallを自作せよ。
; A:
(defn myall [p xs]
	(if (empty? xs)
		true
		(and (p (first xs)) (myall p (rest xs)))))