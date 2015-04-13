; Q: 与えられたシフト数で文字列を暗号化する関数encodeを書け。
; A
(defn int2let [n]
  (char (+ n (int \a))))
(defn let2int [c]
  (- (int c) (int \a)))
(defn shift [n c]
  (if (Character/isLowerCase c)
    (int2let (mod (+ (let2int c) n) 26))
    c))
(defn encode [n cs]
  (apply str (for [c cs] (shift n c))))
