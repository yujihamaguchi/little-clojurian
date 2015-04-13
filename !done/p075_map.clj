; --Q: mapを再帰を用いて自作せよ。
; --A
(defn mymap [f xs]
	(if (empty? xs)
		()
		(cons (f (first xs)) (mymap f (rest xs)))))