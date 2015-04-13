; Q: 1から100までの二乗の和を計算する式をリスト内包表記を用いて書け。
; A
(reduce + (for [n (take 100 (iterate inc 1))] (Math/pow n 2)))
