; Q: 指定した特定の文字がいくつ含まれているか数える関数char-countを書け。
; A
(defn char-count [c cs]
  (count (for [c' cs :when (= c c')] c')))
