; Q: 与えられた上限までに含まれる完全数全てを算出する関数perfectsをリスト内包表記と関数factorsを使って定義せよ。
;    完全数：自分自身をのぞく約数の和が自分自身と等しい整数
; A
(defn factors [n]
	(for [n' (range 1 (inc n)) :when (= 0 (rem n n'))] n'))
(defn perfects [n]
	(for [n' (range 1 (inc n)) :when (= n' (- (reduce + (factors n')) n'))] n'))