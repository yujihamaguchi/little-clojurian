; Q: 文字列から小文字を数える関数lowersを書け。
; A
(defn lowers [cs]
  (count (re-seq #"[a-z]" cs)))

