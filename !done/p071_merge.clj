; Q: 整列されたリストを二つとり、一つの整列されたリストにして返す関数mergeを書け。
;    insertやisort等、整列されたリストを処理する関数は用いてはならない。
;    ex) merge [2,5,6] [1,3,4] ==> [1,2,3,4,5,6]
; A
(defn mymerge [xs ys]
  (if (empty? xs)
    ys
    (if (empty? ys)
      xs
      (if (< (first xs) (first ys))
        (cons (first xs) (mymerge (rest xs) ys))
        (cons (first ys) (mymerge xs (rest ys)))))))