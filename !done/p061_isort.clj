; Q: 関数insertを用いてリストのソートを行う"挿入ソート"を行う関数isortを書け。
; A
(defn myinsert [x xs]
	(if (empty? xs)
		[x]
		(if (<= x (first xs))
			(cons x xs)
			(cons (first xs) (myinsert x (rest xs))))))
(defn isort [xs]
	(if (empty? xs)
		xs
		(myinsert (first xs) (isort (rest xs)))))
