; Q: concatをリスト内包表記で実装したmyconcatを書け。
; A
(defn myconcat [xss]
  (for [xs xss] (for [x xs] x)))