; Q: 暗号化された文字列は手に入れたが、シフト数は分からないとしよう。暗号文を解読するためにシフト数を推測したい。
;    これは次のように実現できる。すなわち暗号文に対する文字の出現頻度表を作り、この表を左に回転させながら、
;    期待される文字の出現頻度表に対するカイ二乗検定の値を計算する。そして、算出されたカイ二乗検定の値のリストの中で、
;    最小の値の位置をシフト数とする。
;    以上を実行する関数crackを書け。
(def table [8.2 1.5 2.8 4.3 12.7 2.2 2.0 6.1 7.0 0.2 0.8 4.0 2.4 6.7 7.5 1.9 0.1 6.0 6.3 9.1 2.8 1.0 2.4 0.2 2.0 0.1])
(defn let2int [c]
	(- (int c) (int \a)))
(defn int2let [n]
	(char (+ n (int \a))))
(defn percent [n m]
	(float (* (float (/ n m)) 100)))
(defn rotate [n cs]
	(concat (drop n cs) (take n cs)))
(defn char-count [c cs]
	(reduce + (for [c' cs :when (= c c')] 1)))
(defn freqs [cs]
	(for [c (map int2let (range 26)) :when (Character/isLowerCase c)] (percent (char-count c cs) (count cs))))
(defn shift [n c]
	(if (Character/isLowerCase c)
		(int2let (mod (+ (let2int c) n) 26))
		c))
(defn chisqr [os es]
	(reduce + (for [[o e] (map vector os es)] (/ (Math/pow (- o e) 2) e))))
(defn indexed [xs]
	(map vector (iterate inc 0) xs))
(defn crack [cs]
  (let [
  	rotated-tables (for [n (range 26)] (rotate n (freqs cs)))
  	indexed-chisqr (for [[idx table'] (indexed rotated-tables)] [idx (chisqr table' table)])
  	n (first (reduce (fn [[i1 c1] [i2 c2]] (if (< c1 c2) [i1 c1] [i2 c2])) indexed-chisqr))]
  	(apply str (map #(shift (- n) %) cs))))
