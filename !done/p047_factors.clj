; Q: 正の整数に対し、すべての約数を計算する関数factorsを書け
; A
(defn factors [n]
	(for [n' (range 1 (inc n)) :when (= 0 (rem n n'))] n'))
