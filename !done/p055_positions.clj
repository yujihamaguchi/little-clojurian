; Q: 関数positionsを関数find(p047)を用いて再定義せよ。
; A
(defn find [key tpls]
	(for [[k v] tpls :when (= key k)] v))
(defn positions [x xs]
	(find x (map vector xs (iterate inc 0))))