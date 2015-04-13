; Q: 偶数の長さを持つリストを半分ずつに分割する関数halveを書け。
; A1
(defn halve [xs]
	(let [n (quot (count xs) 2)]
		(list (apply str (take n xs)) (apply str (drop n xs)))))
; A2
; (defn halve [xs]
;   (let [n (quot (count xs) 2)]
;     (map #(apply str %) (split-at n xs))))
