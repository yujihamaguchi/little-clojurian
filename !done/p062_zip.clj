; Q: zipを再帰を用いて自作せよ。
; A
(defn myzip [xs ys]
	(if (or (empty? xs) (empty? ys))
		()
		(cons (vector (first xs) (first ys)) (myzip (rest xs) (rest ys)))))