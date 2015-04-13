; Q: evenとoddを相互再帰を用いて自作せよ。
;    ヒント：0は偶数
; A
(declare myeven? myodd?)
(defn myeven? [n]
  (if (zero? n)
    true
    (myodd? (dec n))))
(defn myodd? [n]
  (if (zero? n)
    false
    (myeven? (dec n))))
