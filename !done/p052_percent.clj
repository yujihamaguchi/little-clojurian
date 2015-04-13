; Q: 百分率を計算し、浮動小数点数として返す関数percentを書け。
; A
(defn percent [n m]
  (float (* (float (/ n m)) 100)))
