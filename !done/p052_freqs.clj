; Q: 任意の文字列に対して文字の出現頻度表を返す関数freqsを書け。（lowersとcountとpercentを用いる）
; A
(defn lowers [cs]
  (count (for [c cs :when (Character/isLowerCase c)] c)))
(defn int2let [n]
  (char (+ n (int \a))))
(defn char-count [c cs]
  (count (for [c' cs :when (= c c')] c)))
(defn percent [n m]
  (float (* (float (/ n m)) 100)))
(defn freqs [cs]
  (let [n (lowers cs)]
    (for [c (map int2let (range 26))] (percent (char-count c cs) n))))
