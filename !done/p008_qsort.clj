; Q: クイックソート関数qsortを書け
; A
(defn qsort [xs]
	(if (empty? xs)
		()
		(let [
      x (first xs)
			xs (rest xs)
			lt (for [x' xs :when (< x' x)] x')
			ge (for [x' xs :when (>= x' x)] x')]
			(concat (qsort lt) [x] (qsort ge)))))
