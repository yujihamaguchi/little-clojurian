; Q: 二進表記が必ず8ビットになるように切り詰めたり適切な数の0を詰め込んだりする関数make8を書け。
; A
(defn make8 [bs]
	(vec (take 8 (concat bs (replicate 7 0)))))