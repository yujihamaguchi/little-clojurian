; Q: mapをリスト内包表記を用いて自作せよ。
; A
(defn mymap [f xs]
	(for [x xs] (f x)))
