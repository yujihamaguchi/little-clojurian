(ns clj.core
  (:require [clojure.core.match :refer [match]])
  (:require [clojure.core.async :as async :refer [chan >!! <!! close! thread go >! <! alts!! timeout alts!]]))

(def table [8.2 1.5 2.8 4.3 12.7 2.2 2.0 6.1 7.0 0.2 0.8 4.0 2.4 6.7 7.5 1.9 0.1 6.0 6.3 9.1 2.8 1.0 2.4 0.2 2.0 0.1])

;; Q001: haskell の zip と同様の機能の関数 my-zip を書け （パラメータの数は可変であること）
;; zip :: [a] -> [b] -> [(a, b)]
(defn my-zip
  [& colls]
  (when (seq colls)
    (apply (partial map vector) colls)))

;; Q002: haskell の sum と同様の機能の関数 sum を書け。(再帰を用いるパターン, reduce を用いるパターン、 apply を用いるパターン)
;; sum :: (Num a) => [a] -> a
;; sum ns
;;     数値のリスト ns の総和を返す。
;;     see also: product, foldl
;;         sum [1, 2, 3]  = 6
;;         sum []         = 0
(defn sum
  [ns]
  (if-not (seq ns)
    0
    (+ (first ns)
       (sum (rest ns)))))
;; (defn sum [ns]
;;   (reduce + ns))
;; (defn sum [ns]
;;   (apply + ns))

;; Q003: クイックソート関数 qsort01 を書け（リスト内包表記を使うこと）
(defn qsort01
  [xs]
  (if-not (seq xs)
    []
    (let [x (first xs)
          xs' (rest xs)
          lt (for [x' xs' :when (< (int x') (int x))] x')
          gt (for [x' xs' :when (> (int x') (int x))] x')]
      (concat (qsort01 lt) [x] (qsort01 gt)))))

;; Q004: Haskell の product と同様の機能の関数を書け(再帰を用いるパターン、 reduce を用いるパターン、 apply を用いるパターン)
;; product :: (Num a) => [a] -> a
;; product ns
;;     数値のリスト ns の全要素の積を返す。
;;         product [2, 3, 4]   = 24
;;         product [4, 5, 0]   = 0
;;         product []          = 1
;; =================================
;; A: Using recursion.
(defn product [ns]
  (if-not (seq ns)
    1
    (* (first ns) (product (rest ns)))))
;; A: Using reduce.
;; (defn product [ns]
;;   (reduce * ns))
;; A: Using apply.
;; (defn product [ns]
;;   (apply * ns))

;; Q005: リストを逆順に整列する関数 rqsort を書け
(defn rqsort
  [xs]
  (if-not (seq xs)
    []
    (let [x (first xs)
          xs' (rest xs)
          gt (for [x' xs' :when (< (int x) (int x'))] x')
          lt (for [x' xs' :when (> (int x) (int x'))] x')]
      (concat (rqsort gt) [x] (rqsort lt)))))

;; Q006: Haskell の init と同様の機能の関数 my-init を書け(再帰を用いるバージョンも書くこと)
;; init :: [a] -> [a]
;; リスト xs の最後の要素を除いたリストを返す。
;;     init [1,2,3]   = [1,2]
;;     init [1]       = []
(defn my-init [xs]
  ((comp reverse rest reverse) xs))
;; A: Using recursion.
;;(defn my-init [xs]
;;  (if-not (seq (rest xs))
;;    []
;;    (cons (first xs) (my-init (rest xs)))))

;; Q007: Haskell の last と同様の機能の関数 my-last を書け(再帰を用いるバージョンも書くこと)
;; last :: [a] -> a
;;     リストの最後の要素を返す。
;;         last [1,2,3]   = 3
;;         last []        = エラー
(defn my-last
  [xs]
  (if-not (seq xs)
    (throw (java.util.NoSuchElementException.))
    (-> xs
        reverse
        first)))
;; A: Using recursion.
#_(defn my-last
  [xs]
  (if-not (seq xs)
    (throw (java.util.NoSuchElementException.))
    (letfn [(my-last' [xs]
              (if-not (seq (next xs))
                (first xs)
                (my-last' (rest xs))))]
      (my-last' xs))))
;; or
#_(defn my-last
  [xs]
  (cond
    (not (seq xs)) (throw (java.util.NoSuchElementException.))
    (= 1 (count xs)) (first xs)
    :else (my-last (rest xs))))

;; Q008: 偶数の長さを持つリストを半分ずつに分割する関数 halve を書け。
(defn halve
  [xs]
  (let [n (count xs)]
    (if (or (zero? n) (not (zero? (rem n 2))))
      (throw (java.lang.IllegalArgumentException.))
      (let [n' (quot n 2)]
        [(take n' xs) (drop n' xs)]))))

;; Q009: concat をリスト内包表記で実装した my-concat を書け。
;; concat :: [[a]] -> [a]
;; concat xs
;;     リストのリスト xs を一つのリストに連結する。
;;         concat [[1,2], [3,4], [5,6]]    = [1,2,3,4,5,6]
;;         concat ["ab", "cd", "ef"]       = "abcdef"
;;         concat [[]]                     = []
;;         concat []                       = []
(defn my-concat
  [xss]
  (for [xs xss, x xs] x))

;; Q010: 正の整数に対し、すべての約数を計算する関数 factors を書け
(defn factors
  [n]
  (for [n' (range 1 (inc n)) :when (zero? (rem n n'))] n'))

;; Q011: 対( pair )のリストを検索キーで探索し、対応する値を取り出してリストにする関数 my-find をリスト内包表記と分配束縛を用いて書け。
(defn my-find
  [k ps]
  (for [[k' v] ps :when (= k k')] v))

;; Q012: 対のリストから、対の先頭の要素を取り出してリストを生成する firsts をリスト内包表記と分配束縛を用いて書け。
(defn firsts
  [ps]
  (for [[k _] ps] k))

;; Q013: haskell の length を、 sum とリスト内包表記で書け。
;; length :: [a] -> Int
;; length xs
;;     リスト xs の長さを返す。
;;         length [1,2,3]   = 3
;;         length "abcde"   = 5
;;         length []        = 0
;;         length ""        = 0
(defn length
  [xs]
  (sum (for [_ xs] 1)))

;; Q014: factors を用いて、整数が素数か判定する関数 prime? を書け。
(defn prime?
  [n]
  (= [1 n] (factors n)))

;; Q015: prime を用いて与えられた上限数までの全ての素数を生成する関数 primes を書け。
(defn primes
  [n]
  (for [n' (range 2 (inc n)) :when (prime? n')] n'))

;; Q016: リストから隣り合う要素をマップにして返す関数 pairs を zipmap を用いて書け。
;; ex)
;;   [1 2] => {1 2}
;;   [1 2 3] => {1 2, 2 3}
;;   [1 2 3 4] => {1 2, 2 3, 3 4}
(defn pairs
  [xs]
  (zipmap xs (rest xs)))

;; Q017: 順序クラスに属する任意の型の要素を持つリストが、整列されているか調べる関数 sorted? を pairs 関数を用いて書け。
;;       （本来、 pairs のような処理を行いたい場合、 Clojure ではシーケンスライブラリの partition を使用する。）
(defn sorted?
  [xs]
  (every?
   (fn [[x y]] (<= x y))
   (pairs xs)))

;; Q018: 目的とする値がリストのどの位置にあるかを調べて、その位置全てをリストとして返す関数 positions を書け。(index は 0 から開始される事)
(defn positions
  [x xs]
  (for [[i x'] (zipmap (range) xs) :when (= x' x)] i))

;; Q019: 指定した特定の文字がいくつ含まれているか数える関数 char-count を書け。
(defn char-count [c cs]
  (count (filter #(= c %) cs)))
#_(defn char-count
    [c cs]
    (sum (for [c' cs :when (= c' c)] 1)))

;; Q020: 文字列から小文字を数える関数 lower-count を書け。（正規表現を用いたパターンも）
(defn lower-count
  [cs]
  (count (filter #(Character/isLowerCase %) cs)))
#_(defn lower-count
    [cs]
    (count (re-seq #"[a-z]" cs)))

;; Q021: Unicode コードポイント（整数、 'a' を 0 とする）を文字に変換する関数 int2let を書け。
(defn int2let
  [n]
  (char (+ n (int \a))))

;; Q022: 文字を Unicode のコードポイント（整数）に変換する関数 let2int を書け。（ 'a' を 0 とする）
(defn let2int
  [c]
  (- (int c) (int \a)))

;; Q023: 小文字をシフト数だけずらす my-shift を書け。 (循環すること。 'z' に対し、 1 ならば 'a' となる）
(defn my-shift
  [n c]
  (if-not (re-seq #"[a-z]" (str c))
    c
    (let [a2z-letter-count (count (range (let2int \a) (inc (let2int \z))))
          n' (rem (+ n (let2int c)) a2z-letter-count)]
      (int2let n'))))

;; Q024: 与えられたシフト数で文字列を暗号化する関数 my-encode を書け。
(defn my-encode
  [n cs]
  (apply str (map (partial my-shift n) cs)))

;; Q025: 百分率を算出する関数 percent を書け。
(defn percent
  [n m]
  (* (/ n m) 100))

;; Q026: 任意の文字列に対して小文字アルファベットの出現頻度表を返す関数 freqs を書け。（ lower-count と char-count を用いる）
(defn freqs
  [cs]
  (let [tbl (map int2let (range (let2int \a) (inc (let2int \z))))
        cnt (lower-count cs)]
    (for [c tbl]
      (percent (char-count c cs) cnt))))

;; Q027: カイ二乗検定を行う関数 chisqr （期待値のリストが第一引数）を書け。
(defn chisqr
  [observeds expecteds]
  (sum (map (fn [expected observed]
              (float (/ (Math/pow (- observed expected)
                                  2)
                        expected)))
            expecteds
            observeds)))

;; Q028: 文字列を任意の数 n だけ左に回転させる関数 rotate を書け。（先頭の文字は末尾に接続していると考える）
(defn rotate
  [n s]
  (let [n' (rem n (count s))]
    (apply str (concat (drop n' s) (take n' s)))))

;; Q029: 1 から 100 までの二乗の和を計算する関数 sum-square-1-to-100 をリスト内包表記を用いて書け。
(defn sum-square-1-to-100
  []
  (reduce + (for [n (range 1 101)] (* n n))))

;; Q030: 2つの生成器を持つリスト内包表記 [(x,y) | x <- [1,2,3], y <- [4,5,6]] は、
;;       1つの生成器を持つリスト内包表記2つでも表現出来る事を示せ。
(=  (for [x [1 2 3] y [4 5 6]] [x y])
    (apply concat (for [x [1 2 3]] (for [y [4 5 6]] [x y]))))

;; Q031: 与えられた上限の数値までに含まれる完全数全てを算出する関数 perfects をリスト内包表記と関数 factors および sum を使って定義せよ。
;;       完全数：自分自身をのぞく約数の和が自分自身と等しい整数
(defn perfects
  [n]
  (for [n' (range 1 n) :when (= n' (- (sum (factors n')) n'))] n'))

;; Q032: ピタゴラス数のリストを生成する関数 pyths をリスト内包表記を使って定義せよ。
;;       ただし、ピタゴラス数の要素は与えられた上限 n 以下であるとする。
(defn pyths
  [n]
  (letfn [(pyth? [a b c]
            (= (+ (Math/pow a 2) (Math/pow b 2))
               (Math/pow c 2)))]
    (let [ns (range 1 (inc n))]
      (for [a ns
            b ns
            c ns
            :when (and (< a b)
                       (pyth? a b c))]
        [a b c]))))

;; Q033: ある要素のみからなるリストを生成する関数 my-replicate を書け。(直接の再帰、それを使わないバージョンをそれぞれ書け)
;;    ex) >replicate 3 True
;;        [True, True, True]
;; 直接の再帰
(defn my-replicate [n x]
  (if (zero? n)
    []
    (cons x (my-replicate (dec n) x))))
;; 直接の再帰ではない
;; (defn my-replicate [n x]
;;   (letfn [(my-replicate' [n acc]
;;             (if (zero? n)
;;               acc
;;               (my-replicate' (dec n) (cons x acc))))]
;;     (my-replicate' n [])))

;; Q034: 二つの整数のリストの内積を求める関数　scalarproduct　を書け。
;; A
(defn scalarproduct
  [ns ms]
  (reduce + (map #(* %1 %2) ns ms)))

;; Q035: リストの順番を逆転する関数 my-reverse を直接の再帰を用いて書け。
;;       *hint*
;;       vector: indexed なデータ型
;;       list: 連結リスト
(defn my-reverse
  [xs]
  (if-not (seq xs)
    []
    (conj (my-reverse (rest xs)) (first xs))))

;; Q036: ある要素を、整列されたリストに挿入する関数 my-insert を書け。
(defn my-insert
  [x xs]
  (if (or (not (seq xs))
          (<= x (first xs)))
    (cons x xs)
    (cons (first xs) (my-insert x (rest xs)))))

;; Q037: 関数 myinsert を用いてリストのソートを"挿入ソート"で行う関数　isort　を書け。
(defn isort
  [xs]
  (if-not (seq xs)
    xs
    (my-insert (first xs)
               (isort (rest xs)))))

;; Q038: drop を再帰を用いて自作( my-drop )せよ。
(defn my-drop
  [n xs]
  (if (or (zero? n)
          (not (seq xs)))
    xs
    (my-drop (dec n) (rest xs))))

;; Q039: zip を直接の再帰を用いて自作( my-zip )せよ。
(defn my-zip2
  [xs ys]
  (when (and (seq xs) (seq ys))
    (cons [(first xs) (first ys)] (my-zip2 (rest xs) (rest ys)))))

;; Q040: even と odd を相互再帰を用いて自作( my-even?, my-odd? )せよ。( declare を自作( my-declare )してそれを用いること。 if を使わないこと)
;;       ヒント： 0 は偶数、 -3 は奇数
(defmacro my-declare [& expr]
  `(do ~@(map #(list 'def %) expr)))

(my-declare my-even? my-odd?)

(defn my-even? [n]
  (or (zero? n)
      (my-odd? (dec (Math/abs n)))))

(defn my-odd? [n]
  (and (not (zero? n))
       (my-even? (dec (Math/abs n)))))

;; Q041: 0 以上の整数 n に対し、 n 番目のフィボナッチ数を求める関数 fibonacci を書け。（直接の再帰を用いて良い）
(defn fibonacci
  [n]
  (case n
    0 0
    1 1
    (+ (fibonacci (- n 1))
       (fibonacci (- n 2)))))

;; Q042: qsort を再帰を用いて書け。（直接の再帰を用いて良い）
(defn qsort
  [xs]
  (if-not (seq xs)
    []
    (let [x (first xs)
          lt (for [x' xs :when (< x' x)] x')
          gt (for [x' xs :when (> x' x)] x')]
      (concat (qsort lt) [x] (qsort gt)))))

;; Q043: リストから偶数の位置の要素を取り出す関数 evens と、奇数の位置の要素を取り出す関数 odds を相互再帰を用いて書け。
(declare evens odds)
(defn evens
  [xs]
  (if-not (seq xs)
    []
    (odds (rest xs))))
(defn odds
  [xs]
  (if-not (seq xs)
    []
    (cons (first xs) (evens (rest xs)))))

;; Q044: Haskell の init 関数を自作( my-init )せよ。(直接の再帰を用いたもの、遅延評価関数を用いたもの両方書くこと）
;; 再帰版
(defn my-init [coll]
  (if-not (seq (rest coll))
    []
    (cons (first coll) (my-init (rest coll)))))

;; 遅延評価関数版
#_(defn my-init
    [xs]
    (-> xs
        reverse
        rest
        reverse))

;; Q045: Haskell の elem を直接の再帰を用いて自作( my-elem )せよ。
;;       elem :: Eq a => a -> [a] -> Bool
(defn my-elem
  [x xs]
  (when (seq xs)
    (or
     (= x (first xs))
     (my-elem x (rest xs)))))

;; Q046: Haskell の !! の前置記法版である index 関数を直接の再帰を用いて自作せよ。( my-index )
(defn my-index
  [xs n]
  (if (zero? n)
    (first xs)
    (my-index (rest xs) (dec n))))

;; Q047: 整列されたリストを二つとり、一つの整列されたリストにして返す関数 merge を直接の再帰を用いて自作せよ。( my-merge )
;;       insert や isort 等、整列されたリストを処理する関数は用いてはならない。
;;       ex) merge [2,5,6] [1,3,4] ==> [1,2,3,4,5,6]
(defn my-merge
  [xs ys]
  (cond
    (not (seq xs)) ys
    (not (seq ys)) xs
    :default (let [x (first xs)
                   y (first ys)]
               (if (<= x y)
                 (cons x (my-merge (rest xs) ys))
                 (cons y (my-merge xs (rest ys)))))))

;; Q048: 関数 my-merge を用いてマージソートを実行する関数 msort を再帰を用いて書け。
;;       マージソートは、引数のリストを二つに分割し、それぞれを整列した後、再び一つに戻す事で、整列を実現する。
;;       最初に、リストを半分に分割する関数 simple-halve を書け。
(defn simple-halve
  [xs]
  (let [n (quot (count xs) 2)]
    [(take n xs) (drop n xs)]))
(defn msort
  [xs]
  (if-not (seq (next xs))
    xs
    (let [[xs1 xs2] (simple-halve xs)]
      (my-merge (msort xs1) (msort xs2)))))

;; Q049: Haskell の replicate を再帰を用いて自作せよ。( my-replicate-rec )
;;       replicate :: Int -> a -> [a]
(defn my-replicate-rec
  [n x]
  (if (zero? n)
    []
    (cons x (my-replicate-rec (dec n) x))))

;; Q050: 負でない整数に対する累乗演算を行う関数 my を直接の再帰を用いて定義せよ。
(defn my
  [a n]
  (if (zero? n)
    1
    (* a (my a (dec n)))))

;; Q051: map をリスト内包表記を用いて自作せよ( my-map )。
(defn my-map
  [f xs]
  (for [x xs] (f x)))

;; Q052: filterをリスト内包表記を用いて自作せよ。( my-filter )
(defn my-filter
  [f xs]
  (for [x xs :when (f x)] x))

;; Q053: map を再帰を用いて自作せよ。( my-map-recur )
;; 直接の再帰を用いたパターン
(defn my-map-recur
  [f xs]
  (if-not (seq xs)
    []
    (cons (f (first xs))
          (my-map-recur f (rest xs)))))

;; recurを用いたパターン
#_(defn my-map-recur
    [f xs]
    (letfn [(my-map-recur'
              [acc xs]
              (if-not (seq xs)
                acc
                (recur (conj acc (f (first xs)))
                       (rest xs))))]
      (my-map-recur' [] xs)))

;; Q054: リストの先頭から述語を満たす連続した要素を取り除く関数 drop-while を自作せよ。( my-drop-while )
(defn my-drop-while [p xs]
  (if (or (empty? xs) (not (p (first xs))))
    xs
    (my-drop-while p (rest xs))))

;; Q055: filter を再帰を用いて自作せよ。( my-filter-recur )
;;       線形再帰、末尾再帰、 recur を用いた末尾再帰の 3 パターンを書くこと
;; 線形再帰
(defn my-filter-recur
  [p xs]
  (if-not (seq xs)
    []
    (let [x   (first xs)
          xs' (rest xs)]
      (if (p x)
        (cons x (my-filter-recur p xs'))
        (my-filter-recur p xs')))))

;; 末尾再帰
#_(defn my-filter-recur
    [p xs]
    (letfn [(my-filter-recur'
              [acc xs]
              (if-not (seq xs)
                acc
                (let [x   (first xs)
                      xs' (rest xs)]
                  (my-filter-recur' (if (p x) (conj acc x) acc) xs'))))]
      (my-filter-recur' [] xs)))

;; recur を用いた末尾再帰
#_(defn my-filter-recur
    [p xs]
    (loop [acc [] xs xs]
      (if-not (seq xs)
        acc
        (let [x   (first xs)
              xs' (rest xs)]
          (recur (if (p x) (conj acc x) acc) xs')))))

;; Q056: リストの先頭から述語を満たす連続した要素を取り出す関数 takeWhile を自作せよ。( my-take-while )
(defn my-take-while
  [p [x & xs' :as xs]]
  (cond
    (not (seq xs)) []
    (not (p x)) []
    :else (cons x (my-take-while p xs'))))

;; Q057: foldr を自作せよ。（ my-foldr ）
;;       * Haskell では以下のような実装になる。
;;
;;         myFoldr :: (a -> b -> b) -> b -> [a] -> b
;;         myFoldr _ v [] = v
;;         myFoldr f v (x:xs) = f x (myFoldr f v xs)
;;
;;         以下のように、 foldr に部分適用して関数をつくることができる。
;;
;;         cons = foldr (:) []
;;         sum = foldr (+) 0
;;         product = foldr (*) 1
;;         or = foldr (||) False
;;         and = foldr (&&) True
(defn my-foldr
  [f v [x & xs' :as xs]]
  (if-not (seq xs)
    v
    (f x (my-foldr f v xs'))))

;; Q057-01: foldl を自作せよ。（ my-foldl ）
;;          * Haskell では以下のような実装になる。
;;
;;            myFoldr :: (a -> b -> a) -> a -> [b] -> a
;;            myFoldr _ v [] = v
;;            myFoldr f v (x:xs) = foldl f (f v x) xs
(defn my-foldl
  [f v [x & xs' :as xs]]
  (if-not (seq xs)
    v
    (my-foldl f (f v x) xs')))

;; Q057-02: ビットのリストで表現される二進表記を整数に変換する関数 bit->int を書け。
;;    ・ iterateを用いること
;;    ・ 二進表記は逆順であること
;; type Bit = Int
;; bit2int :: [Bit] -> Int
;; bit2int bits = sum [b * w | (b, w) <- zip bits weights]
;;                 where
;;                   weights = iterate (*2) 1
(defn bits->int
  [bs]
  (reduce + (map #(* %1 %2)
                 bs
                 (iterate #(* 2 %) 1))))

;; Q057-03: core.async
;; https://github.com/clojure/core.async/blob/master/examples/walkthrough.clj

;; Q058: 負でない整数を二進表記へ変換する関数 int2bit を書け。( 0 は正の整数ではない)
(defn int->bits [n]
  (if (zero? n)
    []
    (cons (mod n 2) (int->bits (quot n 2)))))

;; Q059: 二進表記が必ず 8 ビットになるように切り詰めたり適切な数の 0 を詰め込んだりする関数 make8 を書け。
(defn make8
  [bs]
  (take 8 (concat bs (repeat 0))))

;; Q060: ビット列を 8 ビットの二進表記に分割する関数 chop8 を書け。
(defn chop8 [bs]
  (if (empty? bs)
    []
    (lazy-seq (cons (make8 (take 8 bs)) (chop8 (drop 8 bs))))))

;; Q061: ビットのリストを文字列に復号する関数 decode を書け。
;;       リストを分割し、二進表記を Unicode のコードポイント（整数）へ変換し、文字へ直して、全体として文字列にする。
(defn decode
  [bs]
  (->> (chop8 bs)
       (map bits->int)
       (map char)
       (apply str)))

;; Q062: 文字列をビット列に符号化する関数 encode を書け。
;;       それぞれの文字列を unicode のコードポイント（整数）に変換し、
;;       さらに 8 ビットの二進表記に直して、全体を連結することで、ビットのリストを作る。
(defn encode
  [s]
  (->> (map int s)
       (map int->bits)
       (mapcat make8)))

;; Q063: 関数 all を自作せよ。( my-all )
;;       all :: (a -> Bool) -> [a] -> Bool
;;       all f xs
;;       xs の要素 x について、f x がすべて True なら True。
;;
;;       all (==1) [5,4,3,2,1]   = False
;;       all (==1) [1,1,1]       = True
;;       all (==1) []            = True
(defn my-all
  [p [x & xs' :as xs]]
  (if-not (seq xs)
    true
    (and (p x)
         (my-all p xs'))))
#_(defn my-all
    [p [x & xs' :as xs]]
    (or (not (seq xs))
        (and (p x)
             (my-all p xs'))))

;; Q064: 関数 any を自作せよ。( my-any )
;;       any :: (a -> Bool) -> [a] -> Bool
;;       any f xs
;;       xs のいずれかの要素 x について f x が True ならば True。
;;
;;       any (== 1) [5, 4, 3, 2, 1]   = True
;;       any (== 1) [5, 4, 1, 2, 3]   = True
;;       any (== 1) [5, 4, 3, 2]      = False
(defn my-any
  [p [x & xs' :as xs]]
  (if-not (seq xs)
    false
    (or (p x)
        (my-any p xs'))))

;; Q065: 暗号化された文字列は手に入れたが、シフト数は分からないとしよう。
;;       暗号文を解読するためにシフト数を推測したい。
;;       これは次のように実現できる。
;;       すなわち暗号文に対する文字の出現頻度表を作り、この表を左に回転させながら、
;;       期待される文字の出現頻度表に対するカイ二乗検定の値を計算する。
;;       そして、算出されたカイ二乗検定の値のリストの中で、最小の値の位置をシフト数とする。
;;
;;       以上を実行する関数 crack を書け。
;;
;; (def table [8.2 1.5 2.8 4.3 12.7 2.2 2.0 6.1 7.0 0.2 0.8 4.0 2.4 6.7 7.5 1.9 0.1 6.0 6.3 9.1 2.8 1.0 2.4 0.2 2.0 0.1])

;; (defn let2int [c]
;;   (- (int c) (int \a)))

;; (defn int2let [n]
;;   (char (+ n (int \a))))

;; (defn percent [n m]
;;   (float (* (float (/ n m)) 100)))

;; (defn rotate [n cs]
;;   (concat (drop n cs) (take n cs)))

;; (defn char-count [c cs]
;;   (reduce + (for [c' cs :when (= c c')] 1)))

;; (defn freqs [cs]
;;   (for [c (map int2let (range 26)) :when (Character/isLowerCase c)] (percent (char-count c cs) (count cs))))

;; (defn shift [n c]
;;   (if (Character/isLowerCase c)
;;     (int2let (mod (+ (let2int c) n) 26))
;;     c))

;; (defn chisqr [os es]
;;   (reduce + (for [[o e] (map vector os es)] (/ (Math/pow (- o e) 2) e))))

;; (defn indexed [xs]
;;   (map vector (iterate inc 0) xs))

;; (defn crack [cs]
;;   (let [
;;     rotated-tables (for [n (range 26)] (rotate n (freqs cs)))
;;     indexed-chisqr (for [[idx table'] (indexed rotated-tables)] [idx (chisqr table' table)])
;;     n (first (reduce (fn [[i1 c1] [i2 c2]] (if (< c1 c2) [i1 c1] [i2 c2])) indexed-chisqr))]
;;     (apply str (map #(shift (- n) %) cs))))

(def alpha-low-case
  (map char (range (int \a) (inc (int \z)))))

(defn shift-string [s n]
  (apply str (map #(my-shift n %) s)))

(defn round-shift-string [s]
  (map #(shift-string s %) (range 0 26)))

(defn indexed [xs]
  (apply hash-map (interleave (iterate inc 0) xs)))

(defn min-val-index [coll]
  (let [v (apply min (vals coll))]
    (first (first (filter #(= v (val %)) coll)))))

(defn freq-table [s]
  (for [c alpha-low-case] (float (/ (count (filter (hash-set c) s)) (count s)))))

(defn guess-shift-count [obs ex]
  (min-val-index (indexed (map #(chisqr (freq-table %) ex) obs))))

(defn crack [s]
  (shift-string s (guess-shift-count (round-shift-string s) table)))

;; Q066: ファイルが過去半時間の間に更新されたかどうか調べる述語 recently-modified? を書け。
;;       （パラメータとして、 java.io.File オブジェクトが渡される想定）
(defn recently-modified?
  [f]
  (>= (* 1000 60 30)
      (- (System/currentTimeMillis)
         (.lastModified f))))

;; #{集合}
(def compositions #{{:name "The Art of the Fugue", :composer "J. S. Bach"}
                    {:name "Requiem", :composer "W. A. Mozart"}
                    {:name "Requiem", :composer "Giuseppe Verdi"}
                    {:name "Musical Offering", :composer "J. S. Bach"}})
(def composers #{{:composer "J. S. Bach" :country "Germany"}
                 {:composer "W. A. Mozart" :country "Austria"}
                 {:composer "Giuseppe Verdi" :country "Italy"}})
(def nations #{{:nation "Germany" :language "Germany"}
               {:nation "Austria" :language "German"}
               {:nation "Italy" :language "Italian"}})

(use 'clojure.set)
;; Q067: compositions のキーワード :name の別名として :title を持つ集合を取得せよ。( set1 関数の戻り値として)
(defn set1
  []
  (rename compositions {:name :title}))

;; Q068: compositions から :nameが "Requiem" のレコードを抽出せよ（ set2 関数の戻り値として）
(defn set2
  []
  (select #(= "Requiem" (:name %)) compositions))

;; Q069: compositions を　:name で射影せよ。（ set3 関数の戻り値として）
(defn set3
  []
  (project compositions [:name]))

;; Q070: compositions と composers を自然結合せよ。（ set4 関数の戻り値として）
(defn set4
  []
  (join compositions composers))

;; Q071: composers と nations を :country と :nation で結合せよ。（ set5 関数の戻り値として）
(defn set5
  []
  (join composers nations {:country :nation}))

;; Q072: compositions から :name が "Requiem" のレコードを抽出し、 composers と自然結合し、 :country キーで射影せよ。（ set6 関数の戻り値として）
(defn set6
  []
  (-> (select #(= "Requiem" (:name %)) compositions)
      (join composers)
      (project [:country])))

;; Q073: 最底部に bottom というシンボルを持つ、任意の n レベルまでネストしたリストを作る deeply-nested 関数を書け。
(defn deeply-nested
  [n]
  (if (zero? n)
    'bottom
    (list (deeply-nested (dec n)))))

;; Q074: 以下のコイントスの結果データ（ :h 表、 :t 裏 ）について、 表が 2 回続けて出たケースをカウントする関数 count-heads-pairs を loop/recur を用いて書け。
;; (count-heads-pairs [:h :t :t :h :h :h])
;; ;= 2
(defn count-heads-pairs
  [rs]
  (loop [rs rs cnt 0]
    (if-not (next rs)
      cnt
      (recur (rest rs)
             (if (every? #(= :h %) (take 2 rs))
               (inc cnt)
               cnt)))))

;; Q075: 以下の変換を行う関数 by-pairs を、 lazy-seq を用いて書け。
;;     変換前： [:h :t :t :h :h :h]
;;     変換後： ((:h :t) (:t :t) (:t :h) (:h :h) (:h :h))
(defn by-pairs
  [coll]
  (if-not (next coll)
    []
    (lazy-seq (cons (take 2 coll) (by-pairs (rest coll))))))

;; Q076: ホフスタッタの男女シーケンスを書け。( f, m )
;;
;; F(0) = 1;; M(0) = 0
;; F(n) = n - M(F(n-1)), n>0
;; M(n) = n - F(M(n-1)), n>0
;;
;; また、メモ化を行い性能を改善せよ。
;;
;;   シーケンス中の1つの値を計算するために、2つの値を最初から計算せねばならず、そのそれぞれについてまた2つずつの値を最初から計算することになる。
;;   メモ化された関数を呼ぶと、それはまず与えられた引数を、過去に計算した入力と出力のマップと比べる。もし引数が過去に与えられていたものであれば、
;;   再び計算をしなくても、直ちに結果を返すことが出来る。
;;
;; また、式の処理時間を計測するマクロを書け（ elapsed-time )
;;
;; また、メモ化はキャッシュが既に作られていれば再帰を途中で止めることができるけれど、
;; キャッシュが空の状態で大きな数に対するmやfを計算しようとすると、
;; キャッシュが作られる前にスタックが溢れてしまう。
;; それを防ぐ為に、関数ではなくシーケンスを見せることでキャッシュが頭から作られるのを保証し、以下を満たす性能を獲得すること。
;;
;;   (is (< (elapsed-time (nth f-seq 250)) 100))
;;   (is (< (elapsed-time (nth m-seq 250)) 100))
;;
(declare f m)

(defn f
  [n]
  (if (zero? n)
    1
    (- n (m (f (dec n))))))

(defn m
  [n]
  (if (zero? n)
    0
    (- n (f (m (dec n))))))

(def f (memoize f))
(def m (memoize m))

(defmacro elapsed-time
  [expr]
  `(let [start# (System/currentTimeMillis)]
     ~expr
     (- (System/currentTimeMillis) start#)))

(def f-seq (map f (range)))
(def m-seq (map m (range)))

;; Q077-01: s-list （シンボルとシンボルのリスト両方を要素に出来るリスト）、 oldsym、 newsym を引数に取り
;;          s-list の中の oldsym をすべて newsym に置き換える関数 replace-symbol を
;;          シンボル（と見られる要素）の置換を行う replace-symbol-expression 関数との相互再帰で書け。
;;          マルチメソッドを用いたパターンも書け。
;;
;;            (replace-symbol '((a b) (((b g r) (f r)) c (d e)) b) 'b 'a)
;;            ;;= ((a a) (((a g r) (f r)) c (d e)) a)
;;
;;          この関数は深くネストした構造をあたえるとスタックを溢れさせる可能性がある。これを避ける為に遅延評価を用いること。
;;
;; A. 相互再帰版
(declare replace-symbol-expression)

(defn replace-symbol
  [s-list oldsym newsym]
  (if-not (seq s-list)
    []
    (lazy-seq (cons (replace-symbol-expression (first s-list)
                                               oldsym
                                               newsym)
                    (replace-symbol (rest s-list)
                                    oldsym
                                    newsym)))))

(defn replace-symbol-expression
  [s-expr oldsym newsym]
  (if (symbol? s-expr)
    (if (= oldsym s-expr) newsym s-expr)
    (replace-symbol s-expr oldsym newsym)))

;; A. マルチメソッド版
;; (defmulti replace-symbol (fn [s-expr _ _] (symbol? s-expr)))

;; (defmethod replace-symbol true
;;   [sym oldsym newsym]
;;   (if (= oldsym sym)
;;     newsym
;;     sym))

;; (defmethod replace-symbol false
;;   [s-list oldsym newsym]
;;   (if-not (seq s-list)
;;     []
;;     (lazy-seq
;;      (cons (replace-symbol (first s-list) oldsym newsym)
;;            (replace-symbol (rest s-list) oldsym newsym)))))

;; Q078: 名前（ username ）をパラメータとし、 "{greeting-prefix}, {username}" の文字列を返す関数を返す、
;;       挨拶の種類（ greeting-prefix ）をパラメータとする関数 make-greeter を書け。
;;
;;       ((make-greeter "Hello") "Yuji")
;;       ;;= "Hello, Yuji"
;;       ((make-greeter "Aloha") "Yuji")
;;       ;;= "Aloha, Yuji"
;;
(defn make-greeter
  [greeting-prefix]
  (fn [username]
    (format "%s, %s" greeting-prefix username)))

;; Q079: n 番目のフィボナッチ数を返す、 recur で明示的な再帰を行う関数 recur-fibo を書け。
;;
;;       (recur-fibo 9)
;;       ;;= 34N
;;       (recur-fibo 1000000)
;;       ;;= 195 ...(中略)... 875N
;;
;;       ※ テストに時間が掛かるため、該当テストをコメントアウトしている
(defn recur-fibo
  [n]
  (loop [i 0 f1 0 f2 1]
    (if (= n i)
      f1
      (recur (inc i)
             f2
             (+' f1 f2)))))

;; Q080: 遅延評価されるフィボナッチ数列を生成する関数 lazy-seq-fibo を書け。
(defn lazy-seq-fibo
  []
  (letfn [(lazy-seq-fibo'
            [n m]
            (lazy-seq (cons n (lazy-seq-fibo' m (+ n m)))))]
    (lazy-seq-fibo' 0 1)))

;; Q081: 指定したディレクトリ、またはファイル以下にある Clojure のソースファイル（ *.clj ）の（空行を除いた）行数の合計をカウントする関数 clojure-loc を書け。
;;       (下記「参考」に目を通し、 slurp を使用しないバージョンも書け)
;;
;;       [参考] https://coderwall.com/p/f1a9xa
;;       WHAT IS SLURP?
;;         slurp is technically a fully realized result of a clojure.java.io/reader.
;;       WHEN SHOULD I USE SLURP?
;;         When memory is not a concern.
;;       WHAT IS READER?
;;         reader will attempt to convert its argument to a BufferedReader.
;;       WHEN SHOULD I USE READER
;;         When a lazy sequence of the results are needed or to create a new BufferedReader.
;;
;; using slurp
(defn clojure-loc
  [f]
  (->> f
       file-seq
       (filter (fn [f]
                 (and (.isFile f)
                      (re-seq #"\.clj" (.getName f)))))
       (map #(->> %
                  slurp
                  (re-seq #"\S")
                  count))
       (reduce +)))

;; using reader
#_(defn clojure-loc
  [f]
  (->> f
       file-seq
       (filter (fn [f]
                 (and (.isFile f)
                      (re-seq #"\.clj" (.getName f)))))
       (map #(with-open [r (clojure.java.io/reader %)]
               (->> r
                    line-seq
                    (filter (complement empty?))
                    count)))
       (reduce +)))

;; Q082: 文字列中の文字で、探すべき文字のセットにマッチする文字のインデックスを得る関数 index-filter を書け。
;;       
;;       ex)
;;         (index-filter #{\a \b} "abcdbbb")
;;         ;;= (0 1 4 5 6)
;;         (index-filter #{\a \b} "xyz")
;;         ;;= ()
;;
(defn index-filter
  [cs s]
  (for [[i c'] (zipmap (range) s)
        :when (cs c')]
    i))

;; Q083: 以下の ./resources/compositions.xml から、作曲家（ composer ）の名前だけを抜き出す関数（ get-composer )を書け。
;;
;; <compositions>
;;   <composition composer="J. S. Bach">
;;     <name>The Art of the Fugue</name>
;;   </composition>
;;   <composition composer="J. S. Bach">
;;     <name>Musical Offering</name>
;;   </composition>
;;   <composition composer="W. A. Mozart">
;;     <name>Requiem</name>
;;   </composition>
;; </compositions>
;;
(require '[clojure.xml :as xml])
(defn get-composer
  [f]
  (->> f
       xml/parse
       xml-seq
       (filter #(-> %
                    :tag
                    (= :composition)))
       (map #(-> %
                 :attrs
                 :composer))))

;; Q083-2: マクロ and、 or を my-and、 my-or として自作せよ。
(defmacro my-and
  ([] true)
  ([b] b)
  ([b & bs]
   `(if-not ~b
      false
      (my-and ~@bs))))

(defmacro my-or
  ([] false)
  ([b] b)
  ([b & bs]
   `(if ~b
      true
      (my-or ~@bs))))

;; Q083-3: 相互再帰を使って、 my-odd? および my-even? を定義せよ。
(declare my-odd?)
(defn my-even?
  [n]
  (if (zero? n)
    true
    #(my-odd? (dec n))))

(defn my-odd?
  [n]
  (if (zero? n)
    false
    #(my-even? (dec n))))

;; Q084: 任意のディレクトリ以下のファイル、ディレクトリ名をシーケンスとして取得する関数list-filesを書け。
;; A
(defn list-files
  [d]
  (map #(.getName %) (.listFiles (java.io.File. d))))

;; Q084-2: Clojureの..マクロを真似するchainマクロを書け。
;;
;; | マクロ呼び出し                     | 展開後                         |
;; | ----------------------------- | ----------------------------- |
;; | (chain arm getHand)           | (. arm getHand)               |
;; | (chain arm getHand getFinger) | (. (. arm getHand) getFinger) |
;;
;; (macroexpand '(chain arm getHand))
;; ;;= (. arm getHand)
;; (macroexpand '(chain arm getHand getFinger))
;; ;;= (. (. arm getHand) getFinger)
;; ;; example
;; (chain " abc " trim length)
;; ;;= 3
(defmacro chain
  ([x form] `(. ~x ~form))
  ([x form & more] `(chain (. ~x ~form) ~@more)))

;; Q085: n番目のフィボナッチ数を返す、末尾再帰を用いたtail-fibo関数を書け。
;; A
;; user=> (tail-fibo 1000000)
;; StackOverflowError   java.math.BigInteger.add (:-1)
;; *末尾再帰で書いたにも関わらず、大きなnを与えるとやはり落ちてしまう。
;;  Haskellのような関数型言語は簡単にTCOを行えるのだが、JVMは関数型言語向けには設計されていないので、
;;  JVM上で直接走る言語で、自動的にTCOを行える言語は存在しない。TCOが無いことは残念なことではあるが、
;;  そのせいで関数型プログラミングが全くできなくなってしまうというわけではない。
;;  Clojureは以下のいくつかの現実的な代替方法を用意している。
;; - recurを使った明示的な自己再帰
;; - 遅延シーケンス
;; - trampolineを使った明示的な相互再帰
(defn tail-fibo
  [i]
  (letfn [(tail-fibo-
            [i n m]
            (if (zero? i)
              n
              (recur (dec i) m (+ n m))))]
    (tail-fibo- i 0 1)))

;; Q086: *out*を一時的に新たなStringWriterに束縛し、exprsを評価して、評価中に*out*へ出力されたものを文字列にして返すwith-out-strマクロを自作せよ。(my-with-out-str)
;; (my-with-out-str (print "hello, ") (print "world"))
;; ;;= "hello, world"
;; refer: [Let vs. Binding in Clojure](http://stackoverflow.com/questions/1523240/let-vs-binding-in-clojure)
(defmacro my-with-out-str
  [& exprs]
  `(binding [*out* (java.io.StringWriter.)]
     (do ~@exprs)
     (str *out*)))

;; Q087: 任意のファイルの空行を除いた行数を表示する関数count-not-empty-lineを書け。
;; ex) (= (count-not-empty-line (java.io.File. "./resources/ut-count-not-empty-line/01.txt")) 3)
;; A
(defn count-not-empty-line
  [f]
  (with-open [r (clojure.java.io/reader f)]
    (->> (line-seq r)
         (filter #(not (clojure.string/blank? %)))
         count)))

;; Q088: 以下の動作をする関数count-runsをpartitionを用いて書け。
;;       （先立って、filterしてcountするcount-if関数を書け。また、関数合成と部分適用も使用せよ。）
;; (count-runs 2 #(= :h %) [:h :t :t :h :h :h])
;; ;;= 2
;; (count-runs 2 #(= :t %) [:h :t :t :h :h :h])
;; ;;= 1
;; (count-runs 3 #(= :h %) [:h :t :t :h :h :h])
;; ;;= 1
(def count-if (comp count filter))
(defn count-runs
  [n p coll]
  (count-if (partial every? p) (partition n 1 coll)))

;; Q089: シーケンスライブラリの関数であるiterateを用いてフィボナッチ数列を生成する関数fiboを書け。
;; この関数は以下のように大きな値に対しても動作する。
;; (take 10 (fibo))
;; ;;= (0 1 1 2 3 5 8 13 21 34)
;; (rem (nth (fibo) 1000000) 1000)
;; ;;= 875N
;; A
(defn fibo
  []
  (map first (iterate (fn [[n m]] [m (+' n m)]) [0 1])))

;; Q090: n番目のフィボナッチ数を返す、単純な再帰を使ったstack-consuming-fibo関数を書け。
;; (stack-consuming-fibo 9)
;; ;;= 34
;; (stack-consuming-fibo 1000000N)
;; ;;= StackOverflowError   clojure.lang.Numbers.toBigInt (Numbers.java:249)
;; *再帰のせいで、 n>1 に対する stack-consuming-fibo 1回の呼び出しはさらに stack-consuming-fibo 2回の呼び出しを引き起こす。
;; JVMレベルではこれらの呼び出しはJavaのメソッド呼び出しになり、各呼び出しごとにスタックフレームというデータ構造がアロケートされる。
;; stack-consuming-fibo は n に比例したスタックフレームを作り、いずれJVMスタックを使い尽くして先の例で見られた StackOverflowError を引き起こす。
;; Clojure の関数呼び出しは常にスタックフレームを作りスタック領域を使うのでスタック消費型と呼ばれる。
;; Clojure では、 stack-consuming-fibo がやっているようなスタックを消費する再帰はほぼ常に避けるべきだ。
;; A
(defn stack-consuming-fibo
  [n]
  (case n
    0 0
    1 1
    (+ (stack-consuming-fibo (- n 1))
       (stack-consuming-fibo (- n 2)))))

;; Q091: 文字列が空白文字だけで構成されているか否か調べるblank?関数を書け。
(defn blank? [cs]
  (every? #(Character/isWhitespace %) cs))
;; my answer 2019-01-31
#_(defn blank?
    [cs]
    (empty? (re-seq #"\w" cs)))

;; Q092: timeマクロの変種で、何回もの実行結果を後で集めやすいようにしたbenchというマクロを書け。
;; ;; (bench (str "a" "b"))
;; {:result "ab", :elapsed 53026}
;; ;; は次のとおり展開される
;; (let [start (System/nanoTime)
;;       result (str "a" "b")]
;;   {:result result :elapsed (- (System/nanoTime) start)})
(defmacro bench
  [expr]
  `(let [start# (System/nanoTime)
         result# ~expr]
     {:result result#, :elapsed (- (System/nanoTime) start#)}))

;; Q093: Write a function whose name is p22 which returns the total number of elements in a sequence.(p22)
;; Special Restrictions
;; don't use count function and use reduce function.
;; (= (__ '(1 2 3 3 1)) 5)
;; (= (__ "Hello World") 11)
;; (= (__ [[1 2] [3 4] [5 6]]) 3)
;; (= (__ '(13)) 1)
;; (= (__ '(:a :b :c)) 3)
(defn p22
  [coll]
  (reduce (fn [acc _] (inc acc)) 0 coll))

;; Q094: Write a function which reverses a sequence.(p23)
;; Special Restrictions
;; 1. Don't use these function below.
;;    - reverse
;;    - rseq
;; 2. Use reduce function.
;; (= (__ [1 2 3 4 5]) [5 4 3 2 1])
;; (= (__ (sorted-set 5 7 2 7)) '(7 5 2))
;; (= (__ [[1 2][3 4][5 6]]) [[5 6][3 4][1 2]])
(defn p23
  [coll]
  (reduce conj '() coll))

;; Q095: Write a function which returns the first X fibonacci numbers.(p26)
;;       Use: iterate -> map first -> take
;; (= (__ 3) '(1 1 2))
;; (= (__ 6) '(1 1 2 3 5 8))
;; (= (__ 8) '(1 1 2 3 5 8 13 21))
;; A:
(defn p26
  [n]
  (take n (map first (iterate (fn [[n m]] [m (+ n m)]) [1 1]))))

;; Q096: Write a function which returns true if the given sequence is a palindrome.(p27)
;; Hint: "racecar" does not equal '(\r \a \c \e \c \a \r)
;; (false? (__ '(1 2 3 4 5)))
;; (true? (__ "racecar"))
;; (true? (__ [:foo :bar :foo]))
;; (true? (__ '(1 1 3 3 1 1)))
;; (false? (__ '(:a :b :c)))
(defn p27 [coll]
  (= (seq coll) (reverse coll)))

;; Q097: Write a function which flattens a sequence.(p28)
;; Special Restrictions
;; flatten
;; (= (__ '((1 2) 3 [4 [5 6]])) '(1 2 3 4 5 6))
;; (= (__ ["a" ["b"] "c"]) '("a" "b" "c"))
;; (= (__ '((((:a))))) '(:a))
;; A:
(defn p28
  [xs]
  (when (seq xs)
    (let [x (first xs)
          xs' (rest xs)]
      (if (coll? x)
        (concat (p28 x) (p28 xs'))
        (cons x (p28 xs'))))))

;; Q098: Write a function which removes consecutive duplicates from a sequence.(p30)
;;       一つは再帰、一つは reduce を用いて解いてみること
;; (= (apply str (__ "Leeeeeerrroyyy")) "Leroy")
;; (= (__ [1 1 2 3 3 2 2 3]) '(1 2 3 2 3))
;; (= (__ [[1 2] [1 2] [3 4] [1 2]]) '([1 2] [3 4] [1 2]))
;; A:
(defn p30
  [xs]
  (when (seq xs)
    (let [x (first xs)
          xs' (rest xs)]
      (cons x (p30 (drop-while #(= x %) xs'))))))

#_(defn p30
    [coll]
    (reduce
     (fn [acc x]
       (if (= (last acc) x)
         acc
         (conj acc x)))
     [] coll))

;; Q099: Write a function which packs consecutive duplicates into sub-lists.(p31)
;;       一つは再帰、一つはパーティション関数を使って
;; (= (__ [1 1 2 1 1 1 3 3]) '((1 1) (2) (1 1 1) (3 3)))
;; (= (__ [:a :a :b :b :c]) '((:a :a) (:b :b) (:c)))
;; (= (__ [[1 2] [1 2] [3 4]]) '(([1 2] [1 2]) ([3 4])))
;; A:
(defn p31 [coll]
  (when (seq coll)
    (let [x (first coll)]
      (cons (take-while #(= x %) coll) (p31 (drop-while #(= x %) coll))))))
#_(defn p31 [coll]
    (partition-by identity coll))

;; Q100: Write a function which duplicates each element of a sequence.(p32)
;;       一つは再帰、一つは mapcat 関数を使って
;; (= (__ [1 2 3]) '(1 1 2 2 3 3))
;; (= (__ [:a :a :b :b]) '(:a :a :a :a :b :b :b :b))
;; (= (__ [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4]))
;; (= (__ [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4]))
(defn p32
  [xs]
  (when (seq xs)
    (let [x (first xs)]
      (concat [x x] (p32 (rest xs))))))

#_(defn p32 [coll]
    (mapcat #(list % %) coll))

;; Q101: Write a function which replicates each element of a sequence a variable number of times.(p33)
;; (= (__ [1 2 3] 2) '(1 1 2 2 3 3))
;; (= (__ [:a :b] 4) '(:a :a :a :a :b :b :b :b))
;; (= (__ [4 5 6] 1) '(4 5 6))
;; (= (__ [[1 2] [3 4]] 2) '([1 2] [1 2] [3 4] [3 4]))
;; (= (__ [44 33] 2) [44 44 33 33])
(defn p33
  [coll n]
  (mapcat #(repeat n %) coll))

;; Q102: Write a function which creates a list of all integers in a given range.(p34)
;; Special Restrictions
;; range
;; (= (__ 1 4) '(1 2 3))
;; (= (__ -2 2) '(-2 -1 0 1))
;; (= (__ 5 8) '(5 6 7))
(defn p34 [n m]
  (when (< n m)
    (cons n (p34 (inc n) m))))

;; Q103: Write a function which takes a variable number of parameters and returns the maximum value.(p38)
;; (= (__ 1 8 3 4) 8)
;; (= (__ 30 20) 30)
;; (= (__ 45 67 11) 67)
(defn p38 [& coll]
  (reduce (fn [n m] (if (< n m) m n)) coll))

;; Q104: Write a function which takes two sequences and returns the first item from each, then the second item from each, then the third, etc.(p39)
;;       一つは再帰、一つは mapcat を使って
;; Special Restrictions
;; interleave
;; (= (__ [1 2 3] [:a :b :c]) '(1 :a 2 :b 3 :c))
;; (= (__ [1 2] [3 4 5 6]) '(1 3 2 4))
;; (= (__ [1 2 3 4] [5]) [1 5])
;; (= (__ [30 20] [25 15]) [30 25 20 15])
(defn p39
  [xs ys]
  (when (and (seq xs) (seq ys))
    (concat [(first xs) (first ys)]
            (p39 (rest xs) (rest ys)))))

#_(defn p39 [xs ys]
    (mapcat list xs ys))

;; Q105: Write a function which separates the items of a sequence by an arbitrary value.(p40)
;;       一つは再帰、一つは mapcat を使う
;; Special Restrictions
;; interpose
;; (= (__ 0 [1 2 3]) [1 0 2 0 3])
;; (= (apply str (__ ", " ["one" "two" "three"])) "one, two, three")
;; (= (__ :z [:a :b :c :d]) [:a :z :b :z :c :z :d])
(defn p40
  [c xs]
  (rest (mapcat #(list c %) xs)))

#_(defn p40
    [c coll]
    (if-not (seq (rest coll))
      coll
      (cons (first coll) (cons c (p40 c (rest coll))))))

;; Q106: Write a function which drops every Nth item from a sequence.(p41)
;; (= (__ [1 2 3 4 5 6 7 8] 3) [1 2 4 5 7 8])
;; (= (__ [:a :b :c :d :e :f] 2) [:a :c :e])
;; (= (__ [1 2 3 4 5 6] 4) [1 2 3 5 6])
(defn p41 [coll n]
  (when (seq coll)
    (concat (take (dec n) coll) (p41 (drop n coll) n))))

;; Q107: Write a function which calculates factorials.(p42)
;; (= (__ 1) 1)
;; (= (__ 3) 6)
;; (= (__ 5) 120)
;; (= (__ 8) 40320)
(defn p42
  [n]
  (if (= n 1)
    1
    (* n (p42 (dec n)))))

;; Q108: Write a function which reverses the interleave process into x number of subsequences.(p43)
;; (= (__ [1 2 3 4 5 6] 2) '((1 3 5) (2 4 6)))
;; (= (__ (range 9) 3) '((0 3 6) (1 4 7) (2 5 8)))
;; (= (__ (range 10) 5) '((0 5) (1 6) (2 7) (3 8) (4 9)))
(defn p43
  [xs n]
  (apply map list (partition n xs)))

;; Q109: Write a function which can rotate a sequence in either direction.(p44)
;; (= (__ 2 [1 2 3 4 5]) '(3 4 5 1 2))
;; (= (__ -2 [1 2 3 4 5]) '(4 5 1 2 3))
;; (= (__ 6 [1 2 3 4 5]) '(2 3 4 5 1))
;; (= (__ 1 '(:a :b :c)) '(:b :c :a))
;; (= (__ -4 '(:a :b :c)) '(:c :a :b))
(defn p44 [n coll]
  (let [n' (mod n (count coll))]
    (concat (drop n' coll) (take n' coll))))
;; my naive answer
;; (defn p44 [n coll]
;;   (cond
;;     (= n 0) coll
;;     (< n 0) (p44 (inc n) (cons (last coll) (drop-last coll)))
;;     :else   (p44 (dec n) (conj (vec (rest coll)) (first coll)))))

;; Q110: Write a function which takes a sequence consisting of items with different types
;;       and splits them up into a set of homogeneous sub-sequences.
;;       The internal order of each sub-sequence should be maintained,
;;       but the sub-sequences themselves can be returned in any order
;;       (this is why 'set' is used in the test cases).(p50)
;; (= (set (__ [1 :a 2 :b 3 :c])) #{[1 2 3] [:a :b :c]})
;; (= (set (__ [:a "foo"  "bar" :b])) #{[:a :b] ["foo" "bar"]})
;; (= (set (__ [[1 2] :a [3 4] 5 6 :b])) #{[[1 2] [3 4]] [:a :b] [5 6]})
(defn p50 [coll]
  (vals (group-by type coll)))

;; Q111: Given a vector of integers, find the longest consecutive sub-sequence of increasing numbers.(p53)
;;       If two sub-sequences have the same length, use the one that occurs first.
;;       An increasing sub-sequence must have a length of 2 or greater to qualify.
;; (= (__ [1 0 1 2 3 0 4 5]) [0 1 2 3])
;; (= (__ [5 6 1 3 2 7]) [5 6])
;; (= (__ [2 3 3 4 5]) [3 4 5])
;; (= (__ [7 6 5 4]) [])
;; Rewrite using funcitons "continuous?" and "take-while".
(defn p53 [coll]
  (letfn [(continuous? [[n m]] (= (inc n) m))]
    (loop [coll coll result []]
      (if-not (seq coll)
        result
        (let [ps (partition 2 1 coll)
              result' (vec (set (apply concat (take-while continuous? ps))))]
          (recur (rest coll) (if (< (count result) (count result')) result' result)))))))
;; my answer 2016/12/24
;;(defn p53 [xs]
;;  (letfn [(continuous? [[n m]] (= (inc n) m))
;;          (concat-linked-pairs [ps]
;;            (if-not (seq (next ps))
;;              (first ps)
;;              (cons (first (first ps)) (concat-linked-pairs (rest ps)))))
;;          (p53' [xs]
;;            (if-not (seq xs)
;;              []
;;              (let [ps' (partition 2 1 xs)
;;                    ps'' (drop-while (complement continuous?) ps')]
;;                (letfn [(continuous? [[n m]] (= (inc n) m))]
;;                  (cons (concat-linked-pairs (take-while continuous? ps'')) (p53' (concat-linked-pairs (drop-while continuous? ps''))))))))]
;;    (reduce (fn [xs ys] (if (>= (count xs) (count ys)) xs ys)) '() (p53' xs))))

;; Q112: Write a function which returns a sequence of lists of x items each. Lists of less than x items should not be returned.(p54)
;; Special Restrictions
;; partition
;; partition-all
;; (= (__ 3 (range 9)) '((0 1 2) (3 4 5) (6 7 8)))
;; (= (__ 2 (range 8)) '((0 1) (2 3) (4 5) (6 7)))
;; (= (__ 3 (range 8)) '((0 1 2) (3 4 5)))
(defn p54 [n coll]
  (if (< (count coll) n)
    []
    (cons (take n coll) (p54 n (drop n coll)))))

;; Q113: Write a function which returns a map containing the number of occurences of each distinct item in a sequence.(p55)
;; Special Restrictions
;; frequencies
;; (= (__ [1 1 2 3 2 1 1]) {1 4, 2 2, 3 1})
;; (= (__ [:b :a :b :a :b]) {:a 2, :b 3})
;; (= (__ '([1 2] [1 3] [1 3])) {[1 2] 1, [1 3] 2})
;; my answer 2019/05/02
(defn p55
  [xs]
  (reduce (fn [acc [k vs]] (assoc acc k (count vs)))
          {}
          (group-by identity xs)))
#_(defn p55 [coll]
    (let [coll' (group-by identity coll)]
      (zipmap (keys coll') (map count (vals coll')))))
;; my naive solution 2014/12/26
;; (defn p55 [coll]
;;   (let [coll' (group-by identity coll)
;;         ks (keys coll')]
;;     (reduce merge (map (fn [n] (hash-map n (count (coll' n)))) ks))))

;; Q114: Write a function which removes the duplicates from a sequence. Order of the items must be maintained.(p56)
;;       1. Use recursion.
;;       2. Use reduce function.
;; (= (__ [1 2 1 3 1 2 4]) [1 2 3 4])
;; (= (__ [:a :a :b :b :c :c]) [:a :b :c])
;; (= (__ '([2 4] [1 2] [1 3] [1 3])) '([2 4] [1 2] [1 3]))
;; (= (__ (range 50)) (range 50))
;; Special Restrictions
;; distinct
;; A.1
(defn p56
  [xs]
  (when (seq xs)
    (let [x (first xs)]
      (cons x (p56 (filter #(not (= x %)) xs))))))
;; A.2
#_(defn p56
    [xs]
    (reduce (fn [acc x] (if (some #{x} acc) acc (conj acc x))) [] xs))

;; Q115: Write a function which allows you to create function compositions.
;;       The parameter list should take a variable number of functions,
;;       and create a function applies them from right-to-left.(p58)
;; Special Restrictions
;; comp
;; (= [3 2 1] ((__ rest reverse) [1 2 3 4]))
;; (= 5 ((__ (partial + 3) second) [1 2 3 4]))
;; (= true ((__ zero? #(mod % 8) +) 3 5 7 9))
;; (= "HELLO" ((__ #(.toUpperCase %) #(apply str %) take) 5 "hello world"))
(defn p58 [& fs]
  (let [[f & fs] (reverse fs)]
    (fn [& xs]
      (reduce (fn [acc f] (f acc)) (apply f xs) fs))))

;; Q116: Take a set of functions and return a new function that takes a variable number of arguments
;;       and returns a sequence containing the result of applying
;;       each function left-to-right to the argument list.(p59)
;; Special Restrictions
;; juxt
;; (= [21 6 1] ((__ + max min) 2 3 5 1 6 4))
;; (= ["HELLO" 5] ((__ #(.toUpperCase %) count) "hello"))
;; (= [2 6 4] ((__ :a :c :b) {:a 2, :b 4, :c 6, :d 8 :e 10}))
(defn p59
  [& fs]
  (fn [& xs]
    (map #(apply % xs) fs)))

;; Q117: Write a function which behaves like reduce,
;;       but returns each intermediate value of the reduction.
;;       Your function must accept either two or three arguments,
;;       and the return sequence must be lazy.(p60)
;; Special Restrictions
;; reductions
;; (= (take 5 (__ + (range))) [0 1 3 6 10])
;; (= (__ conj [1] [2 3 4]) [[1] [1 2] [1 2 3] [1 2 3 4]])
;; (= (last (__ * 2 [3 4 5])) (reduce * 2 [3 4 5]) 120)
(defn p60
  ([f init args]
   (if-not (seq args)
     [init]
     (lazy-seq (cons init (p60 f (f init (first args)) (rest args))))))
  ([f args] (p60 f (first args) (rest args))))

;; Q118: Write a function which returns the first x number of prime numbers.(p67)
;; (= (__ 2) [2 3])
;; (= (__ 5) [2 3 5 7 11])
;; (= (last (__ 100))
(defn p67
  [n]
  (letfn [(factors [n] (filter #(= 0 (mod n %)) (range 1 (inc n))))
          (prime? [n] (= [1 n] (factors n)))
          (primes [] (filter prime? (iterate inc 2)))]
    (take n (primes))))

;; Q119: Write a function which takes a function f and a variable number of maps.
;;       Your function should return a map that consists of the rest of the maps conj-ed onto the first.
;;       If a key occurs in more than one map,
;;       the mapping(s) from the latter (left-to-right) should be combined with the mapping in the result by calling
;;       (f val-in-result val-in-latter)(p69)
;; Special Restrictions
;; merge-with
;;
;; (= (__ * {:a 2, :b 3, :c 4} {:a 2} {:b 2} {:c 5})
;;    {:a 4, :b 6, :c 20})
;; (= (__ - {1 10, 2 20} {1 3, 2 10, 3 15})
;;    {1 7, 2 10, 3 15})
;; (= (__ concat {:a [3], :b [6]} {:a [4 5], :c [8 9]} {:b [7]})
;;    {:a [3 4 5], :b [6 7], :c [8 9]})
(defn p69
  [f m & ms]
  (reduce (fn [acc [k v]]
            (assoc acc k (if-let [v' (acc k)]
                           (f v' v)
                           v)))
          m
          (apply merge {} ms)))
;; my answer 2017/02/18
;; (defn p69 [f & ms]
;;   (letfn [(merge-by-key [m1 m2 k]
;;                         {k (f (k m1) (k m2))})
;;           (merge-m [m1 m2]
;;                   (let [ks (clojure.set/union (keys m1) (kyes m2))]
;;                         (map (partial merge-by-key m1 m2) ks)))]
;;     (reduce merge-m (first ms) (rest ms))))

;; Q120: Given a string of comma separated integers,
;;       write a function which returns a new comma separated string that
;;       only contains the numbers which are perfect squares.(p74)
;; (= (__ "4,5,6,7,8,9") "4,9")
;; (= (__ "15,16,25,36,37") "16,25,36")
(defn p74 [s]
  (->> s
       (re-seq #"\d+")
       (filter (fn [n] (zero? (mod (Math/sqrt (Integer/parseInt n)) 1))))
       (interpose ",")
       (apply str)))

;; Q121: Two numbers are coprime if their greatest common divisor equals 1.
;;       Euler's totient function f(x) is defined as the number of positive integers less than x which are coprime to x.
;;       The special case f(1) equals 1. Write a function which calculates Euler's totient function.(p75)
;;       * ふつうに解いたら、 factors を使って解く方法も試してみる
;; (= (__ 1) 1)
;; (= (__ 10) (count '(1 3 7 9)) 4)
;; (= (__ 40) 16)
;; (= (__ 99) 60)
;; refer:
;;   [Euler's totient function](https://ja.wikipedia.org/wiki/%E3%82%AA%E3%82%A4%E3%83%A9%E3%83%BC%E3%81%AE%CF%86%E9%96%A2%E6%95%B0)
;;   [GCD](http://en.wikipedia.org/wiki/Greatest_common_divisor#Using_Euclid.27s_algorithm)
(defn p75
  [n]
  (if (= n 1)
    1
    (letfn [(gcd [n m]
              (match [n m]
                     [n 0] n
                     :else (gcd m (mod n m))))
            (coprime? [n m] (= 1 (gcd n m)))]
      (count (filter (partial coprime? n) (range 1 n))))))
;; (defn p75 [n]
;;   (letfn [(gcd [n m]
;;               (clojure.set/intersection (set (factors n)) (set (factors m))))
;;           (coprime? [n m]
;;               (= #{1} (gcd n m)))]
;;     (count (filter (fn [m] ((partial coprime? n) m)) (range 1 (inc n))))))

;; Q122: Write a function which finds all the anagrams in a vector of words.
;;       A word x is an anagram of word y if all the letters in x can be rearranged in a different order to form y.
;;       Your function should return a set of sets, where each sub-set is a group of words which are anagrams of each other.
;;       Each sub-set should have at least two words. Words without any anagrams should not be included in the result.(p77)
;; (= (__ ["meat" "mat" "team" "mate" "eat"])
;;    #{#{"meat" "team" "mate"}})
;; (= (__ ["veer" "lake" "item" "kale" "mite" "ever"])
;;    #{#{"veer" "ever"} #{"lake" "kale"} #{"mite" "item"}})
(defn p77
  [xs]
  (->> (group-by sort xs)
       vals
       (filter #(< 1 (count %)))
       (map set)
       set))

;; Q123: Reimplement the function described in "Intro to Trampoline".(p78)
;; Special Restrictions
;; trampoline
;; (= (letfn [(triple [x] #(sub-two (* 3 x)))
;;           (sub-two [x] #(stop? (- x 2)))
;;           (stop? [x] (if (> x 50) x #(triple x)))]
;;     (__ triple 2))
;;   82)
;; (= (letfn [(my-even? [x] (if (zero? x) true #(my-odd? (dec x))))
;;           (my-odd? [x] (if (zero? x) false #(my-even? (dec x))))]
;;     (map (partial __ my-even?) (range 6)))
;;   [true false true false true false])
(defn p78 [f & args]
  (let [f' (apply f args)]
    (if-not (fn? f')
      f'
      (p78 f'))))

;; Q124: A number is "perfect" if the sum of its divisors equal the number itself.
;;       6 is a perfect number because 1+2+3=6.
;;       Write a function which returns true for perfect numbers and false otherwise.(p80)
;; (= (__ 6) true)
;; (= (__ 7) false)
;; (= (__ 496) true)
;; (= (__ 500) false)
;; (= (__ 8128) true)
(defn p80 [n]
  (let [factors (for [n' (range 1 n) :when (zero? (mod n n'))] n')]
    (= n (reduce + factors))))
;; my answer 2017/04/07
;; (defn p80 [n]
;;   (= n (reduce + (drop-last (factors n)))))

;; Q125: Happy numbers are positive integers that follow a particular formula:
;;       take each individual digit, square it, and then sum the squares to get a new number.
;;       Repeat with the new number and eventually, you might get to a number whose squared sum is 1.
;;       This is a happy number. An unhappy number (or sad number) is one that loops endlessly.
;;       Write a function that determines if a number is happy or not.(p86)
;;       この記事のプログラム例を参考にする(https://en.wikipedia.org/wiki/Happy_number)
;; (= (__ 7) true)
;; (= (__ 986543210) true)
;; (= (__ 2) false)
;; (= (__ 3) false)
(defn p86 [n]
  (letfn [(_ [acc n]
            (let [ns (map #(Integer/parseInt (str %)) (str n))
                  n (reduce + (map #(* % %) ns))]
              (if (= 1 n)
                true
                (if (some #(= n %) acc)
                  false
                  (_ (cons n acc) n)))))]
    (_ [] n)))
;; my answer 2017/04/15
;; (defn p86 [n]
;;   (letfn [(convert-digits [n]
;;                           (map #(Character/digit % 10) (str n)))
;;           (sum-sq [ns]
;;                   (reduce + (map #(* % %) ns)))
;;           (is-happy [ns n]
;;             (let [n (sum-sq (convert-digits n))]
;;               (or (= n 1)
;;                   (and (not (contains? ns n)) (is-happy (conj ns n) n)))))]
;;     (is-happy #{} n)))

;; Q126: Write a predicate which checks whether or not a given sequence represents a binary tree.
;;       (Bottom leaf node's value is always nil)
;;       Each node in the tree must have a value, a left child, and a right child.(p95)
;; (= (__ '(:a (:b nil nil) nil))
;;    true)
;; (= (__ '(:a (:b nil nil)))
;;    false)
;; (= (__ [1 nil [2 [3 nil nil] [4 nil nil]]])
;;    true)
;; (= (__ [1 [2 nil nil] [3 nil nil] [4 nil nil]])
;;    false)
;; (= (__ [1 [2 [3 [4 nil nil] nil] nil] nil])
;;    true)
;; (= (__ [1 [2 [3 [4 false nil] nil] nil] nil])
;;    false)
;; (= (__ '(:a nil ()))
;;    false)
(defn p95
  ([] false)
  ([t]
   (if (and (coll? t) (= (count t) 3))
     (let [[v l r] t]
       (and (or (nil? l) (p95 l)) (or (nil? r) (p95 r))))
     false))
  ([a b & rest] false))
#_(defn p95 [n]
    (or (nil? n)
        (and (coll? n)
             (= 3 (count n))
             (let [[n' n1 n2] n]
               (and
                (p95 n1)
                (p95 n2))))))

;; Q127: Let us define a binary tree as "symmetric" if the left half of the tree is the mirror image of the right half of the tree.
;;       Write a predicate to determine whether or not a given binary tree is symmetric.
;;       (see To Tree, or not to Tree for a reminder on the tree representation we're using).(p96)
;; (= (__ '(:a (:b nil nil) (:b nil nil))) true)
;; (= (__ '(:a (:b nil nil) nil)) false)
;; (= (__ '(:a (:b nil nil) (:c nil nil))) false)
;; (= (__ [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
;;           [2 [3 nil [4 [6 nil nil] [5 nil nil]]] nil]])
;;    true)
;; (= (__ [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
;;           [2 [3 nil [4 [5 nil nil] [6 nil nil]]] nil]])
;;    false)
;; (= (__ [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
;;           [2 [3 nil [4 [6 nil nil] nil]] nil]])
;;    false)
(defn p96
  [tr]
  (letfn [(reverse-tree [[p l r]]
            [p
             (when r (reverse-tree r))
             (when l (reverse-tree l))])]
    (= tr (reverse-tree tr))))

;; Q128: Pascal's triangle is a triangle of numbers computed using the following rules:
;;       - The first row is 1.
;;       - Each successive row is computed by adding together adjacent numbers in the row above, and adding a 1 to the beginning and end of the row.
;;       Write a function which returns the nth row of Pascal's Triangle.(p97)
;; (= (__ 1) [1])
;; (= (map __ (range 1 6))
;;    [     [1]
;;         [1 1]
;;        [1 2 1]
;;       [1 3 3 1]
;;      [1 4 6 4 1]])
;; (= (__ 11)
;;    [1 10 45 120 210 252 210 120 45 10 1])
(defn p97 [n]
  (letfn [(p-triangle [coll]
            (lazy-seq (cons coll (p-triangle (concat [1] (map (partial reduce +) (partition 2 1 coll)) [1])))))]
    (nth (p-triangle [1]) (dec n))))
;; my answer 2017/05/14
;; (defn p97 [n]
;;   (case n
;;         1 [1]
;;         2 [1 1]
;;         (concat [1] (map (fn [[n m]] (+ n m)) (partition 2 1 (p97 (dec n)))) [1])))
;;
;; my answer 2019/07/03
;; (defn p97
;;   [n]
;;   (letfn [(combination [n k]
;;             (/ (reduce * (range (inc (- n k)) (inc n)))
;;                (reduce * (range 1 (inc k)))))]
;;     (map (partial combination (dec n)) (range n))))

;; Q129: A function f defined on a domain D induces an equivalence relation on D,as follows:
;;       a is equivalent to b with respect to f if and only if (f a) is equal to (f b).
;;       Write a function with arguments f and D that computes the equivalence classes of D with respect to f.(p98)
;; (= (__ #(* % %) #{-2 -1 0 1 2})
;;    #{#{0} #{1 -1} #{2 -2}})
;; (= (__ #(rem % 3) #{0 1 2 3 4 5 })
;;    #{#{0 3} #{1 4} #{2 5}})
;; (= (__ identity #{0 1 2 3 4})
;;    #{#{0} #{1} #{2} #{3} #{4}})
;; (= (__ (constantly true) #{0 1 2 3 4})
;;    #{#{0 1 2 3 4}})
(defn p98 [f coll]
  (set (map set (vals (group-by f coll)))))

;; Q130: Write a function which calculates the least common multiple.
;;       Your function should accept a variable number of positive integers or ratios.(p100)
;; (== (__ 2 3) 6)
;; (== (__ 5 3 7) 105)
;; (== (__ 1/3 2/5) 2)
;; (== (__ 3/4 1/6) 3/2)
;; (== (__ 7 5/7 2 3/5) 210)
(defn p100 [& rs]
  (letfn [(gcd [n m]
            (if (= n m)
              n
              (if (> n m)
                (gcd m (- n m))
                (gcd n (- m n)))))
          (lcm [n m]
            (/ (* n m) (gcd n m)))]
    (reduce lcm rs)))

;; Q131: When working with java, you often need to create an object with fieldsLikeThis,
;;       but you'd rather work with a hashmap that has :keys-like-this until it's time to convert.
;;       Write a function which takes lower-case hyphen-separated strings and converts them to camel-case strings.(p102)
;;       * Multi-arityを用いたものも書くこと
;; (= (__ "something") "something")
;; (= (__ "multi-word-key") "multiWordKey")
;; (= (__ "leaveMeAlone") "leaveMeAlone")
(defn p102
  [s]
  (letfn [(_p102
            ([c] [c])
            ([c c' & cs] (if (= \- c)
                           (apply _p102 (cons (Character/toUpperCase c') cs))
                           (cons c (apply _p102 (cons c' cs))))))]
    (->> (apply _p102 s)
         (apply str))))
;; my answer 2017/06/10
;; (defn p102 [s]
;;   (clojure.string/replace s #"-([a-z])" #(clojure.string/upper-case (%1 1))))
;; (defn p102 [s]
;;   (clojure.string/replace s #"-[a-z]" (comp clojure.string/upper-case last)))

;; Q132: A balanced number is one whose component digits have the same sum on the left and right halves of the number.
;;       Write a function which accepts an integer n, and returns true iff n is balanced.(p115)
;; (= true (__ 11))
;; (= true (__ 121))
;; (= false (__ 123))
;; (= true (__ 0))
;; (= false (__ 88099))
;; (= true (__ 89098))
;; (= true (__ 89089))
;; (= (take 20 (filter __ (range)))
;;    [0 1 2 3 4 5 6 7 8 9 11 22 33 44 55 66 77 88 99 101])
(defn p115 [n]
  (let [ns (str n)
        m (Math/ceil (/ (count ns) 2))]
    (= (reduce + (map int (take m ns)))
       (reduce + (map int (take m (reverse ns)))))))

(defn map-sum
  [acc tr]
  (when (seq tr)
    (let [n (first tr)
          ns (rest tr)]
      (if (coll? n)
        (cons (map-sum acc n) (map-sum acc ns))
        (cons {(+ acc n) n} (map-sum (+ acc n) ns))))))

(defn filter-sum
  [m tr]
  (println tr)
  (when (seq tr)
    (let [n (first tr)
          ns (rest tr)]
      (println "n: " n)
      (println "ns: " ns)
      (if (map? n)
        (let [acc (first (keys n))
              v (first (vals n))]
          (println "acc: " acc)
          (println "v: " v)
          (if (<= acc m)
            (cons v (filter-sum m ns))
            (filter-sum m ns)))
        (cons (filter-sum m n) (filter-sum m ns))))))

;; Q133: Your friend Joe is always whining about Lisps using the prefix notation for math.
;;       Show him how you could easily write a function that does math using the infix notation.
;;       Is your favorite language that flexible, Joe?
;;       Write a function that accepts a variable length mathematical expression consisting of numbers and the operations +, -, *, and /.
;;       Assume a simple calculator that does not do precedence and instead just calculates left to right.(p135)
;; (= 7  (__ 2 + 5))
;; (= 42 (__ 38 + 48 - 2 / 2))
;; (= 8  (__ 10 / 2 - 1 * 2))
;; (= 72 (__ 20 / 2 + 2 + 4 + 8 - 6 - 10 * 9))
(defn p135
  [n & coll]
  (reduce (fn [acc [f n]]
            (f acc n))
          n
          (partition 2 coll)))

;; Q134: Because Clojure's for macro allows you to "walk" over multiple sequences in a nested fashion,
;;       it is excellent for transforming all sorts of sequences.
;;       If you don't want a sequence as your final output (say you want a map),
;;       you are often still best-off using for, because you can produce a sequence and feed it into a map, for example.
;;       For this problem, your goal is to "flatten" a map of hashmaps.
;;       Each key in your output map should be the "path"1 that you would have to take in the original map to get to a value,
;;       so for example {1 {2 3}} should result in {[1 2] 3}. You only need to flatten one level of maps: if one of the values is a map,
;;       just leave it alone.
;;       1 That is, (get-in original [k1 k2]) should be the same as (get result [k1 k2])(p146)
;;       (= (__ '{a {p 1, q 2}
;;                b {m 3, n 4}})
;;          '{[a p] 1, [a q] 2
;;            [b m] 3, [b n] 4})
;;       (= (__ '{[1] {a b c d}
;;                [2] {q r s t u v w x}})
;;          '{[[1] a] b, [[1] c] d,
;;            [[2] q] r, [[2] s] t,
;;            [[2] u] v, [[2] w] x})
;;       (= (__ '{m {1 [a b c] 3 nil}})
;;          '{[m 1] [a b c], [m 3] nil})
(defn p146 [coll]
  (into {}
        (for [[k v] coll
              [k' v] v]
          [[k k'] v])))
;; (defn p146 [coll]
;;   (apply hash-map
;;     (mapcat
;;       (fn [k coll']
;;         (mapcat
;;           (fn [[k' v]]
;;             (cons (vector k k') [v]))
;;           coll'))
;;       (keys coll)
;;       (vals coll))))

;; Q135: Write a function that, for any given input vector of numbers, returns an infinite lazy sequence of vectors,
;;       where each next one is constructed from the previous following the rules used in Pascal's Triangle. For example,
;;       for [3 1 2], the next row is [3 4 3 2].
;;       Beware of arithmetic overflow! In clojure (since version 1.3 in 2011),
;;       if you use an arithmetic operator like + and the result is too large to fit into a 64-bit integer,
;;       an exception is thrown. You can use +' to indicate that you would rather overflow into Clojure's slower, arbitrary-precision bigint.(p147)
;;
;;      (= (second (__ [2 3 2])) [2 5 5 2])
;;      (= (take 5 (__ [1])) [[1] [1 1] [1 2 1] [1 3 3 1] [1 4 6 4 1]])
;;      (= (take 2 (__ [3 1 2])) [[3 1 2] [3 4 3 2]])
;;      (= (take 100 (__ [2 4 2])) (rest (take 101 (__ [2 2]))))
(defn p147 [coll]
  (lazy-seq
   (cons coll
         (p147 (concat [(first coll)]
                       (map (fn [[n m]] (+' n m)) (partition 2 1 coll))
                       [(last coll)])))))

;; Q136: Write a function which generates the power set of a given set.
;;       The power set of a set x is the set of all subsets of x, including the empty set and x itself.(p85)
;;       hint: 母集合の全ての要素に所与の集合の要素を一つずつ付け加えると Power Set になる。
;;             #{#{}} に 1を付け加えると、 #{#{} #{1}} となり、それに2を付け加えると、 #{#{} #{1} #{2} #{1 2}} となり、それに3を付け加えると、 #{#{} #{1} #{2} #{1 2} #{3} #{1 3} #{2 3} #{1 2 3}} ...
;;             
;;
;;       (= (__ #{1 :a}) #{#{1 :a} #{:a} #{} #{1}})
;;       (= (__ #{}) #{#{}})
;;       (= (__ #{1 2 3})
;;         #{#{} #{1} #{2} #{3} #{1 2} #{1 3} #{2 3} #{1 2 3}})
;;       (= (count (__ (into #{} (range 10)))) 1024)
(defn p85
  [xs]
  (reduce (fn [acc x]
            (into acc (map #(conj % x) acc)))
          #{#{}} xs))

;; Q137: Given an input sequence of keywords and numbers, create a map such that each key in the map is a keyword,
;;       and the value is a sequence of all the numbers (if any) between it and the next keyword in the sequence.(p105)
;; (= {} (__ []))
;; (= {:a [1]} (__ [:a 1]))
;; (= {:a [1], :b [2]} (__ [:a 1, :b 2]))
;; (= {:a [1 2 3], :b [], :c [4]} (__ [:a 1 2 3 :b :c 4]))
;;
;; ※ reduce を使ったパターンと 再帰 を使ったパターン両方書くこと。
(defn p105
  [xs]
  (reduce (fn [acc x]
            (if (keyword? x)
              (assoc acc x [])
              (let [[k _] (last acc)]
                (merge-with conj acc {k x}))))
          {}
          xs))

#_(defn p105
    [[x & xs]]
    (if-not (keyword? x)
      {}
      (assoc
       (p105 (drop-while (complement keyword?) xs))
       x (take-while (complement keyword?) xs))))

;; Q138: Write a function which returns a sequence of digits of a non-negative number (first argument) in numerical system
;;       with an arbitrary base (second argument). Digits should be represented with their integer values,
;;       e.g. 15 would be [1 5] in base 10, [1 1 1 1] in base 2 and [15] in base 16. (p137)
;;       ref: https://ameblo.jp/taku-spi/entry-10539939101.html
;; (= [1 2 3 4 5 0 1] (__ 1234501 10))
;; (= [0] (__ 0 11))
;; (= [1 0 0 1] (__ 9 2))
;; (= [1 0] (let [n (rand-int 100000)](__ n n)))
;; (= [16 18 5 24 15 1] (__ Integer/MAX_VALUE 42))
(defn p137
  [n base]
  (let [result (rem n base)
        n' (quot n base)]
    (if (zero? n')
      [result]
      (conj (p137 n' base) result))))

;;(defn p137 [n m]
;;  (if (zero? n)
;;    [0]
;;    (let [ns (reverse (take-while (fn [k] (<= k n)) (map (fn [l] (Math/pow m l)) (range))))]
;;      (letfn [(_ [ns n]
;;                (if-not (seq ns)
;;                  []
;;                  (cons (int (quot n (first ns)))
;;                        (_ (rest ns) (rem n (first ns))))))]
;;        (_ ns n)))))

;; Q139: Write a function that returns a lazy sequence of "pronunciations" of a sequence of numbers.
;;       A pronunciation of each element in the sequence consists of the number of repeating identical numbers and the number itself.
;;       For example, [1 1] is pronounced as [2 1] ("two ones"), which in turn is pronounced as [1 2 1 1] ("one two, one one").
;;       Your function should accept an initial sequence of numbers, and return an infinite lazy sequence of pronunciations,
;;       each element being a pronunciation of the previous element.(p110)
;; (= [[1 1] [2 1] [1 2 1 1]] (take 3 (__ [1])))
;; (= [3 1 2 4] (first (__ [1 1 1 4 4])))
;; (= [1 1 1 3 2 1 3 2 1 1] (nth (__ [1]) 6))
;; (= 338 (count (nth (__ [3 2]) 15)))
(defn p110
  [coll]
  (let [e (->> coll
               (partition-by identity)
               (mapcat (fn [coll] [(count coll) (first coll)])))]
    (lazy-seq (cons e (p110 e)))))

;; Q140: Write an oscillating iterate: a function that takes an initial value and a variable number of functions.
;;       It should return a lazy sequence of the functions applied to the value in order,
;;       restarting from the first function after it hits the end.(p144)
;; (= (take 3 (__ 3.14 int double)) [3.14 3 3.0])
;; (= (take 5 (__ 3 #(- % 3) #(+ 5 %))) [3 0 5 2 7])
;; (= (take 12 (__ 0 inc dec inc dec inc)) [0 1 0 1 0 1 2 1 2 1 2 3])
(defn p144
  [n & fs]
  (reductions (fn [n f] (f n)) n (cycle fs)))

#_(defn p144
    [n & fs]
    (letfn [(p144' [n fs]
              (let [f (first fs)
                    fs' (rest fs)]
                (lazy-seq (cons n (p144' (f n) fs')))))]
      (p144' n (cycle fs))))

;; Q141: Given any number of sequences, each sorted from smallest to largest, find the smallest single number which appears in all of the sequences.
;;       The sequences may be infinite, so be careful to search lazily.(p108)
;; (= 3 (__ [3 4 5]))
;; (= 4 (__ [1 2 3 4 5 6 7] [0.5 3/2 4 19]))
;; (= 7 (__ (range) (range 0 100 7/6) [2 3 5 7 11 13]))
;; (= 64 (__ (map #(* % % %) (range)) ;;;; perfect cubes
;;           (filter #(zero? (bit-and % (dec %))) (range)) ;;;; powers of 2
;;           (iterate inc 20))) ;;;; at least as large as 20
(defn p108
  [& nss]
  (let [ns (map first nss)
        n (apply min ns)]
    (if (= n (apply max ns))
      n
      (apply p108 (map (fn [ns] (drop-while #(<= % n) ns)) nss)))))

;; Q142: Write a function which flattens any nested combination of sequential things (lists, vectors, etc.),
;;       but maintains the lowest level sequential items. The result should be a sequence of sequences with only one level of nesting.(p93)
;; (= (__ [["Do"] ["Nothing"]])
;;    [["Do"] ["Nothing"]])
;; (= (__ [[[[:a :b]]] [[:c :d]] [:e :f]])
;;    [[:a :b] [:c :d] [:e :f]])
;; (= (__ '((1 2)((3 4)((((5 6)))))))
;;    '((1 2)(3 4)(5 6)))
(defn p93
  [coll]
  (when (seq coll)
    (letfn [(nested-coll? [coll]
              (coll? (first coll)))]
      (let [x (first coll)
            xs (rest coll)]
        (if (nested-coll? x)
          (concat (p93 x) (p93 xs))
          (cons x (p93 xs)))))))

;; Q143: Write a function that accepts a curried function of unknown arity n. Return an equivalent function of n arguments.
;;       You may wish to read this.(p158)
;;       再帰を用いるパターンと、reduceを用いるパターン両方書くこと
;; (= 10 ((__ (fn [a]
;;              (fn [b]
;;                (fn [c]
;;                  (fn [d]
;;                    (+ a b c d))))))
;;        1 2 3 4))
;; (= 24 ((__ (fn [a]
;;              (fn [b]
;;                (fn [c]
;;                  (fn [d]
;;                    (* a b c d))))))
;;        1 2 3 4))
;; (= 25 ((__ (fn [a]
;;              (fn [b]
;;                (* a b))))
;;        5 5))
(defn p158
  [f]
  (fn [& xs]
    (reduce (fn [f x] (f x)) f xs)))
;; (defn p158 [f]
;;   (fn [& coll]
;;     (letfn [(_ [f coll]
;;               (if-not (seq (rest coll))
;;                 (f (first coll))
;;                 (_ (f (first coll)) (rest coll))))]
;;       (_ f coll))))
;; (defn p158
;;   [f]
;;   (partial (fn p158' [f' & xs]
;;              (if-not (seq xs)
;;                f'
;;                (apply p158' (f' (first xs)) (rest xs)))) f))

;; Q144: take-while is great for filtering sequences, but it limited: you can only examine a single item of the sequence at a time.
;;       What if you need to keep track of some state as you go over the sequence?
;;       Write a function which accepts an integer n, a predicate p, and a sequence.
;;       It should return a lazy sequence of items in the list up to, but not including, the nth item that satisfies the predicate.(p114)
;;
;; (= [2 3 5 7 11 13]
;;    (__ 4 #(= 2 (mod % 3))
;;          [2 3 5 7 11 13 17 19 23]))
;; (= ["this" "is" "a" "sentence"]
;;    (__ 3 #(some #{\i} %)
;;          ["this" "is" "a" "sentence" "i" "wrote"]))
;; (= ["this" "is"]
;;    (__ 1 #{"a"}
;;          ["this" "is" "a" "sentence" "i" "wrote"]))
(defn p114 [n p coll]
  (letfn [(_ [acc n coll]
            (if (zero? n)
              (reverse (rest acc))
              (let [x (first coll)
                    xs (rest coll)]
                (_ (cons x acc)
                   (if (p x) (dec n) n)
                   xs))))]
    (_ [] n coll)))
;; my answer 2017/10/30
#_(defn p114
    [n p coll]
    (letfn [(p114'
              [n coll acc]
              (if (zero? n)
                (reverse (rest (reverse acc)))
                (let [x (first coll)
                      xs (rest coll)]
                  (if (p x)
                    (p114' (dec n) xs (conj acc x))
                    (p114' n xs (conj acc x))))))]
      (p114' n coll [])))


;; Q145: Write a function that takes a two-argument predicate, a value, and a collection
;;       and returns a new collection
;;       where the value is inserted between every two items that satisfy the predicate.(p132)
;; (= '(1 :less 6 :less 7 4 3) (__ < :less [1 6 7 4 3]))
;; (= '(2) (__ > :more [2]))
;; (= [0 1 :x 2 :x 3 :x 4]  (__ #(and (pos? %) (< % %2)) :x (range 5)))
;; (empty? (__ > :more ()))
;; (= [0 1 :same 1 2 3 :same 5 8 13 :same 21]
;;    (take 12 (->> [0 1]
;;                  (iterate (fn [[a b]] [b (+ a b)]))
;;                  (map first) ;; fibonacci numbers
;;                  (__ (fn [a b] ;; both even or both odd
;;                        (= (mod a 2) (mod b 2)))
;;                      :same))))
#_(defn p132
    [p a coll]
    (if-not (seq (rest coll))
      coll
      (let [coll' (map (fn [[x y]] (if (p x y) [x a y] [x y])) (partition 2 1 coll))
            x' (first coll')
            xs' (map rest (rest coll'))]
        (reduce concat x' xs'))))
;;; 上記では integer overflow を起こすので、明示的遅延評価を使えるように再帰で書き直す
(defn p132 [p v coll]
  (when (seq coll)
    (let [[x y] (take 2 coll)]
      (if (nil? y)
        [x]
        (lazy-seq
         (concat
          (if (p x y) [x v] [x])
          (p132 p v (rest coll))))))))

;; Q146: This is the inverse of Problem 92, but much easier. Given an integer smaller than 4000,
;;    return the corresponding roman numeral in uppercase, adhering to the subtractive principle.(p104)
;;    http://www.numericana.com/answer/roman.htm#valid
;; (= "I" (__ 1))
;; (= "XXX" (__ 30))
;; (= "IV" (__ 4))
;; (= "CXL" (__ 140))
;; (= "DCCCXXVII" (__ 827))
;; (= "MMMCMXCIX" (__ 3999))
;; (= "XLVIII" (__ 48))
(defn p104 [n]
  (letfn [(_ [n c1 c2 c3]
            (cond
              (< n 4) (repeat n c1)
              (= n 4) [c1 c2]
              (= n 9) [c1 c3]
              (> n 4) (cons c2 (repeat (- n 5) c1))))]
    (let [th (quot n 1000)
          r (rem n 1000)
          h (quot r 100)
          r (rem r 100)
          t (quot r 10)
          u (rem r 10)]
      (apply str
             (concat
              (repeat th \M)
              (_ h \C \D \M)
              (_ t \X \L \C)
              (_ u \I \V \X))))))


;; Q147: You can assume that the input will be well-formed, in upper-case, and follow the subtractive principle.
;;    You don't need to handle any numbers greater than MMMCMXCIX (3999), the largest number representable with ordinary letters.(p92)
;; (= 14 (__ "XIV"))
;; (= 827 (__ "DCCCXXVII"))
;; (= 3999 (__ "MMMCMXCIX"))
;; (= 48 (__ "XLVIII"))
(defn p92 [rn]
  (letfn [(_ [acc rn]
            (if-not (seq rn)
              acc
              (cond
                (re-seq #"^M+" rn) (_ (+ acc (* 1000 (count (take-while #(= \M %) rn)))) (apply str (drop-while #(= \M %) rn)))
                (re-seq #"^CD" rn) (_ (+ acc 400) (apply str (drop 2 rn)))
                (re-seq #"^CM" rn) (_ (+ acc 900) (apply str (drop 2 rn)))
                (re-seq #"^C+" rn) (_ (+ acc (* 100 (count (take-while #(= \C %) rn)))) (apply str (drop-while #(= \C %) rn)))
                (re-seq #"^D+" rn) (_ (+ acc 500 (* 100 (count (take-while #(= \C %) (rest rn))))) (apply str (drop-while #(= \C %) (rest rn))))
                (re-seq #"^XL" rn) (_ (+ acc 40) (apply str (drop 2 rn)))
                (re-seq #"^XC" rn) (_ (+ acc 90) (apply str (drop 2 rn)))
                (re-seq #"^X+" rn) (_ (+ acc (* 10 (count (take-while #(= \X %) rn)))) (apply str (drop-while #(= \X %) rn)))
                (re-seq #"^L+" rn) (_ (+ acc 50 (* 10 (count (take-while #(= \X %) (rest rn))))) (apply str (drop-while #(= \X %) (rest rn))))
                (re-seq #"^IV" rn) (_ (+ acc 4) (apply str (drop 2 rn)))
                (re-seq #"^IX" rn) (_ (+ acc 9) (apply str (drop 2 rn)))
                (re-seq #"^I+" rn) (_ (+ acc (* 1 (count (take-while #(= \I %) rn)))) (apply str (drop-while #(= \I %) rn)))
                (re-seq #"^V+" rn) (_ (+ acc 5 (* 1 (count (take-while #(= \I %) (rest rn))))) (apply str (drop-while #(= \I %) (rest rn))))
                :else 0)))]
    (_ 0 rn)))

;; Q148: Given a sequence S consisting of n elements generate all k-combinations of S,
;;    i. e. generate all possible sets consisting of k distinct elements taken from S.
;;    The number of k-combinations for a sequence is equal to the binomial coefficient.(p103)
;;    [k-combinations](https://en.wikipedia.org/wiki/Combination)
;;    [binomial coefficient](https://en.wikipedia.org/wiki/Binomial_coefficient)
;; (= (__ 1 #{4 5 6}) #{#{4} #{5} #{6}})
;; (= (__ 10 #{4 5 6}) #{})
;; (= (__ 2 #{0 1 2}) #{#{0 1} #{0 2} #{1 2}})
;; (= (__ 3 #{0 1 2 3 4}) #{#{0 1 2} #{0 1 3} #{0 1 4} #{0 2 3} #{0 2 4}
;;                          #{0 3 4} #{1 2 3} #{1 2 4} #{1 3 4} #{2 3 4}})
;; (= (__ 4 #{[1 2 3] :a "abc" "efg"}) #{#{[1 2 3] :a "abc" "efg"}})
;; (= (__ 2 #{[1 2 3] :a "abc" "efg"}) #{#{[1 2 3] :a} #{[1 2 3] "abc"} #{[1 2 3] "efg"}
;;                                     #{:a "abc"} #{:a "efg"} #{"abc" "efg"}})
(defn p103
  [n es]
  (letfn [(gen-tree
            [n es]
            (if (zero? n)
              []
              (map
               (fn [p]
                 (cons p
                       (gen-tree (dec n)
                                 (filter #(not= p %) es))))
               es)))
          (walk-tree-n-depth
            [n tree acc]
            (if (zero? n)
              [acc]
              (mapcat
               (fn [sub-tree]
                 (walk-tree-n-depth
                  (dec n)
                  (when (seq? sub-tree) (rest sub-tree))
                  (conj acc (if (seq? sub-tree) (first sub-tree) sub-tree))))
               tree)))]
    (apply hash-set (walk-tree-n-depth n
                                       (gen-tree n es)
                                       #{}))))

;; Q149: A balanced prime is a prime number which is also the mean of the primes directly before and after it in the sequence of valid primes.
;;    Create a function which takes an integer n, and returns true iff it is a balanced prime.(p116)
;; (= false (__ 4))
;; (= true (__ 563))
;; (= 1103 (nth (filter __ (range)) 15))
(defn factors [n] (for [n' (range 1 (inc n)) :when (zero? (mod n n'))] n'))
(def factors' (memoize factors))
(defn prime? [n] (= [1, n] (factors' n)))
(def prime?' (memoize prime?))
(defn pre-prime [n] (if (or (<= n 0) (prime?' n)) n (pre-prime (dec n))))
(def pre-prime' (memoize pre-prime))
(defn post-prime [n] (if (prime? n) n (post-prime (inc n))))
(def post-prime' (memoize post-prime))
(defn p116 [n]
  ;;  (let [factors (memoize (fn [n] (for [n' (range 1 (inc n)) :when (zero? (mod n n'))] n')))
  ;;        prime? (memoize (fn [n] (= [1, n] (factors n))))]
  (and (prime?' n)
       ;;      (let [pre-prime (memoize (fn [n] (if (or (<= n 0) (prime? n)) n (pre-prime (dec n)))))
       ;;            post-prime (memoize (fn [n] (if (prime? n) n (post-prime (inc n)))))]
       (= n (/ (+ (pre-prime' (dec n)) (post-prime' (inc n))) 2))))

;; テスト無し(REPL)
;; Q150: 2つの文字列から文字を取り出して交互にはさみこめ。またそれを元に戻せ。
;; A:
;; (apply str (interleave "abc" "xyz"))
;; (apply str (take-nth 2 "axbycz"))
;;
;; Q151: 以下の数列をloop/recurを用いて生成せよ。
;;    [5 4 3 2 1]
;; A:
;; (loop [acc [] n 5]
;;   (if (zero? n)
;;     acc
;;     (recur (conj acc n) (dec n))))
;;
;; Q152: declareマクロを自作せよ。
;; A:
;; (defmacro my-declare [& expr]
;;   `(do ~@(map #(list 'def % nil) expr)))
;; FIXME
;; - 4Clojure
;; *To ANKI*
;; https://www.4clojure.com/problem/5 <-> https://www.4clojure.com/problem/7
;; https://www.4clojure.com/problem/11
;; https://www.4clojure.com/problem/47
;; - gpsoftのclojure本でわからなかったところをリストして復習->トレーニング化

(def my-number (ref 0))
(def smallest (ref 1))
(def biggest (ref 100))


(defn guess-my-number []
  (dosync (ref-set my-number (quot (+ (deref smallest) (deref biggest)) 2)))
  (println (deref my-number)))

(defn smaller []
  (dosync (ref-set biggest (deref my-number)))
  (guess-my-number))

(defn bigger []
  (dosync (ref-set smallest (deref my-number)))
  (guess-my-number))

(defrecord Planet [name volume])

(defrecord Planet [
                   name
                   moons
                   volume                                   ;; km^3
                   mass                                     ;; kg
                   aphelion                                 ;; km, farthest from sun
                   perihelion                               ;; km, closest to sun
                   ])

;; Positional factory function
(def earth
  (->Planet "Earth" 1 1.08321e12 5.97219e24 152098232 147098290))
;; Map factory function
(def earth
  (map->Planet {
                :name       "Earth"
                :moons      1
                :volume     1.08321e12
                :mass       5.97219e24
                :aphelion   152098232
                :perihelion 147098290}))

;; 作業用
(defn is-matched-partial? []
  (let [ss (read-string (slurp "./resources/item-keys.txt"))]
    (spit "./resources/compare-result.txt" (apply str (for [[n s1 s2] ss] (str n "\t" (if (empty? (clojure.set/intersection s1 s2)) "FALSE" "TRUE") "\n"))))))

;; Q153: resources ディレクトリ配下の config.edn から都市名(city)を抽出する関数 configured-city を書け
#_(defn configured-city
    []
    (-> (clojufre.java.io/resource "config.edn")
        slurp
        load-string
        :addr
        :city))

(defprotocol Shape
  (area [this]))

(deftype Rectangle [width height]
  Shape
  (area [this] (* width height)))

(deftype Circle [radius]
  Shape
  (area [this] (* radius radius Math/PI)))

(let [c (chan 10)]
  (>!! c "hello")
  (assert (= "hello" (<!! c)))
  (close! c))

(let [c (chan)]
  (thread (>!! c "hello"))
  (println (class (<!! c)))
  (close! c))

(let [c (chan)]
  (go (>! c "hello"))
  (println (class (<!! (go (<! c)))))
  (close! c))
