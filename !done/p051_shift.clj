; Q: 小文字をシフト数だけずらすshiftを書け。（循環すること。ex) 'z'に対し、1ならば'a'となる）（小文字のみ対照とすること）
; A
(defn let2int [c]
  (- (int c) (int \a)))
(defn int2let [n]
  (char (+ (int \a) n)))
(defn shift [n c]
  (if (Character/isLowerCase c)
    (int2let (mod (+ (let2int c) n) 26))
    c))
