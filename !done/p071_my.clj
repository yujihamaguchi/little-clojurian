; Q: 負でない整数に対する累乗演算を行うmyを定義せよ。
; A
(defn my [n m]
  (if (zero? m)
    1
    (* n (my n (dec m)))))
