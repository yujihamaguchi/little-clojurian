; Q: Unicodeコードポイント（整数、'a'が0）を文字に変換する関数int2letを書け。
; A
(defn int2let [n]
  (char (+ (int \a) n)))
