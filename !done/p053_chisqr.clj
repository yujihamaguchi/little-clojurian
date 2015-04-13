; Q: カイ二乗検定を行う関数chisqrを書け。
; A
(defn zip [& colls]
  (when (every? (complement empty?) colls)
    (cons (vec (map first colls))
          (apply zip (map rest colls)))))
(defn chisqr [os es]
  (reduce + (for [[o e] (zip os es)] (/ (Math/pow (- o e) 2) e))))
