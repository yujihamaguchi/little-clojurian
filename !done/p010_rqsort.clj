; Q: リストを逆順に整列する関数rqsortを書け
; A
(defn rqsort [xs]
	(if (empty? xs)
		()
		(let [
			x (first xs)
			xs (rest xs)
			lt (for [x' xs :when (< x' x)] x')
			ge (for [x' xs :when (>= x' x)] x')]
			(concat (rqsort ge) [x] (rqsort lt)))))
