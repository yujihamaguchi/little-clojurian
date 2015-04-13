; --Q: 0以上の整数nに対し、n番目のフィボナッチ数を求める関数fibonacciを書け。
; --A
(defn fibonacci [n]
  (case n
    0 0
    1 1
    (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))