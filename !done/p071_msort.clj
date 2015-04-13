; Q: 関数mergeを用いてマージソートを実行する関数msortを再帰を用いて書け。
;    マージソートは、引数のリストを二つに分割し、それぞれを整列した後、再び一つに戻す事で、整列を実現する。
;    最初に、リストを半分に分割する関数halveを書け。
; A
(defn halve [xs]
  (split-at (quot (count xs) 2) xs))
(defn mymerge [xs ys]
  (if (empty? xs)
    ys
    (if (empty? ys)
      xs
      (if (< (first xs) (first ys))
        (cons (first xs) (mymerge (rest xs) ys))
        (cons (first ys) (mymerge xs (rest ys)))))))
(defn msort
  ([] ())
  ([xs] (if (= 1 (count xs))
    xs
    (let [[x y] (halve xs)]
      (mymerge (msort x) (msort y))))))