; Q: 文字リストの要素をnだけ左に回転させる関数rotateを書け。（リストの先頭は末尾に接続していると考える）
; A
(defn rotate [n cs]
  (apply str (concat (drop n cs) (take n cs))))
