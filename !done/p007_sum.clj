; Q: sumを自作しmysumを書け。
; A
(defn mysum [ns]
  (if (empty? ns)
    0
    (+ (first ns) (mysum (rest ns)))))
