; Q: ピタゴラス数のリストを生成する関数pythsをリスト内包表記を使って定義せよ。ただし、ピタゴラス数の要素は与えられた上限n以下であるとする。
; A
(defn pyths [n]
	(for [
    x (range 1 (inc n))
    ,y (range 1 (inc n))
    ,z (range 1 (inc n))
    :when
      (=
        (Math/pow z 2)
        (+
          (Math/pow x 2)
          (Math/pow y 2)))]
    [x y z]))
