; Q: factorsを用いて、整数が素数か判定する関数primeを書け。
; A
(defn factors [n]
	(for [n' (range 1 (+ n 1)) :when (= 0 (rem n n'))] n'))
(defn prime [n]
	(= [1 n] (vec (factors n))))
