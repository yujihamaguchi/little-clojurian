; Q: 文字をUnicodeのコードポイント（整数）に変換する関数let2intを書け。（'a'が0番とする）
; A
(defn let2int [c]
  (- (int c) (int \a)))
