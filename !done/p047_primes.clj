; Q: primeを用いて与えられた上限数までの全ての素数を生成する関数primesを書け。
; A
(defn factors [n]
	(for [n' (range 1 (+ n 1)) :when (= 0 (rem n n'))] n'))
(defn prime? [n]
	(= [1, n] (factors n)))
(defn primes [n]
	(for [n' (range 1 (+ n 1)) :when (prime n')] n'))