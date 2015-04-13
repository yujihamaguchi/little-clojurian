; Q: lengthをsumとリスト内包表記で書くmylengthを書け。
; A
(defn mylength [ns]
	(apply + (for [n ns] 1)))
