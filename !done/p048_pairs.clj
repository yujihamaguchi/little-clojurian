; Q: リストから隣り合う要素を組にして、リストとして返す関数pairsをzipを用いて書け。
; A
(use '[useful.seq :only (zip)])
(defn pairs [xs]
  (zip xs (rest xs)))
