; Q: filterをリスト内包表記を用いて自作せよ。
; A
(defn myfilter [p xs]
	(for [x xs :when (p x)] x))