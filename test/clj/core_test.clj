(ns clj.core-test
  (:require [clojure.test :refer :all]
    [clj.core :refer :all]))

(deftest ut-zip
  (testing
    (is　(= (zip [1 2] "xy" "ab") '((1 \x \a) (2 \y \b)) ))
    (is　(= (zip [1 2] "x") '((1 \x)) ))
    (is　(= (zip [1] "xy") '((1 \x)) ))
    (is　(= (zip) nil ))
    (is　(or (= (zip "") nil) (= (zip "") '())))
    (is　(or (= (zip "" [1]) nil ) (= (zip "" [1]) '())))
    (is　(or (zip [1] "") nil ) (= (zip [1] "") '()))
  )
)

(deftest ut-sum
  (testing
    (is　(= (sum [1]) 1))
    (is　(= (sum [1 2]) 3))
    (is　(= (sum []) 0 ))
  )
)

(deftest ut-qsort01
  (testing
    (is　(= (qsort01 [3 2 1]) [1 2 3]))
    (is　(= (qsort01 [2 3 1]) [1 2 3]))
    (is　(= (qsort01 [1 2 3]) [1 2 3]))
    (is　(= (qsort01 "cba") '(\a \b \c)))
    (is　(= (qsort01 "bca") '(\a \b \c)))
    (is　(= (qsort01 "abc") '(\a \b \c)))
    (is　(= (qsort01 '(1)) '(1)))
    (is　(= (qsort01 "a") '(\a)))
    (is　(= (qsort01 "") ()))
    (is　(= (qsort01 ()) ()))
  )
)

(deftest ut-product
  (testing
    (is　(= (product [1 2 3]) 6))
    (is　(= (product [1]) 1))
    (is　(= (product []) 1))
  )
)

(deftest ut-rqsort
  (testing
    (is　(= (rqsort [3 2 1]) [3 2 1]))
    (is　(= (rqsort [2 3 1]) [3 2 1]))
    (is　(= (rqsort [1 2 3]) [3 2 1]))
    (is　(= (rqsort "cba") '(\c \b \a)))
    (is　(= (rqsort "bca") '(\c \b \a)))
    (is　(= (rqsort "abc") '(\c \b \a)))
    (is　(= (rqsort '(1)) '(1)))
    (is　(= (rqsort "a") '(\a)))
    (is　(= (rqsort "") ()))
    (is　(= (rqsort ()) ()))
  )
)

(deftest ut-my-init
  (testing
    (is　(= (my-init [1 2 3]) [1 2]))
    (is　(= (my-init [1 2]) [1]))
    (is　(= (my-init [1]) []))
  )
)

(deftest ut-last
  (testing
    (is　(= (my-last [1 2 3]) 3))
    (is　(= (my-last [1 2]) 2))
    (is　(= (my-last [1]) 1))
  )
)

(deftest ut-halve
  (testing
    (is　(= (halve [1 2]) [[1] [2]]))
    (is　(= (halve [1 2 3 4]) [[1 2] [3 4]]))
  )
)

(deftest ut-concat
  (testing
    (is (= (my-concat [1 2] [3 4]) [1 2 3 4]))
    (is (= (my-concat [1 2] [3]) [1 2 3]))
    (is (= (my-concat [1 2]) [1 2]))
  )
)

(deftest ut-factors
  (testing
    (is (= (factors 10) [1 2 5 10]))
    (is (= (factors 5) [1 5]))
    (is (= (factors 1) [1]))
    (is (= (factors 0) []))
  )
)

(deftest und
  (testing
    (is (= (my-find 4 [ [1 \a] [2 \b] ]) []))
    (is (= (my-find 1 [ [1 \a] [2 \b] ]) [\a]))
    (is (= (my-find 1 [ [2 \b] [1 \a] ]) [\a]))
    (is (= (my-find 1 [ [2 \b] [1 \a] [3 \c] ]) [\a]))
    (is (= (my-find 1 [ [1 \a] [1 \b] [3 \c] ]) [\a \b]))
    (is (= (my-find \a [ [\a 1] [\b 2] [\c 3] ]) [1]))
    (is (= (my-find "abc" [ ["abc" 1] ["def" 2] ]) [1]))
  )
)

(deftest ut-firsts
  (testing
    (is (= (firsts [[1]]) [1]))
    (is (= (firsts [[1 2]]) [1]))
    (is (= (firsts [[1 2 3]]) [1]))
    (is (= (firsts [[1] [2]]) [1 2]))
    (is (= (firsts [[1 2] [3 4]]) [1 3]))
    (is (= (firsts [[1 2 3] [4 5 6]]) [1 4]))
    (is (= (firsts ["abc" "def"]) [\a \d]))
  )
)

(deftest ut-length
  (testing
    (is (= (length []) 0))
    (is (= (length [1]) 1))
    (is (= (length [1 2]) 2))
    (is (= (length "abc") 3))
    (is (= (length #{1 2 3}) 3))
    (is (= (length {:a 1 :b 2 :c 3}) 3))
  )
)

(deftest ut-prime
  (testing
    (is (not (prime 1)))
    (is (prime 2))
    (is (prime 3))
    (is (not (prime 4)))
    (is (prime 5))
    (is (not (prime 6)))
  )
)

(deftest ut-primes
  (testing
    (is (= (primes 1) '()))
    (is (= (primes 5) '(2 3 5)))
  )
)

(deftest ut-pairs
  (testing
    (is (= (pairs [1 2]) {1 2}))
    (is (= (pairs [1 2 3]) {1 2, 2 3}))
    (is (= (pairs [1 2 3 4]) {1 2, 2 3, 3 4}))
    (is (= (pairs [1]) {}))
    (is (= (pairs []) {}))
    (is (= (pairs "abc") {\a \b, \b \c}))
  )
)

(deftest ut-sorted
  (testing
    (is (sorted [1 2]))
    (is (not (sorted [2 1])))
    (is (sorted [1]))
    (is (sorted []))
  )
)

(deftest ut-positions
  (testing
    (is (= (positions 1 [1 2]) [0]))
  )
)

(deftest ut-char-count
  (testing
    (is (= (char-count \a "abc") 1))
    (is (= (char-count \b "abc") 1))
    (is (= (char-count \c "abc") 1))
    (is (= (char-count \c "abca") 1))
    (is (= (char-count \c "abbca") 1))
    (is (= (char-count \c "abbccac") 3))
  )
)

(deftest ut-lowers
  (testing
    (is (= (lowers "abcあ亜ア") 3))
    (is (= (lowers "あ亜アAbc") 2))
    (is (= (lowers "aBcあ亜ア") 2))
    (is (= (lowers "あ亜アabC") 2))
  )
)

(deftest ut-int2let
  (testing
    (is (= (int2let 0) \a))
    (is (= (int2let 1) \b))
    (is (= (int2let 2) \c))
  )
)

(deftest ut-let2int
  (testing
    (is (= (let2int \a) 0))
    (is (= (let2int \b) 1))
    (is (= (let2int \c) 2))
  )
)

(deftest ut-shift
  (testing
    (is (= (shift 1 \a) \b))
    (is (= (shift 1 \z) \a))
  )
)

(deftest ut-my-encode
  (testing
    (is (= (my-encode 1 "abc") "bcd"))
    (is (= (my-encode 1 "xyz") "yza"))
  )
)

(deftest ut-chisqr
  (testing
    (is (= (chisqr [1 2 3] [4 5 6]) 5.549999952316284))
    (is (= (chisqr [1 2 3] [4 5]) 4.049999952316284))
  )
)

(deftest ut-perfects
  (testing
    (is (= (perfects 10) [6]))
    (is (= (perfects 100) [6 28]))
  )
)

(deftest ut-pyths
  (testing
    (is (= (pyths 10) [[5 3 4] [10 6 8]]))
  )
)

(deftest ut-my-replicate
  (testing
    (is (= (my-replicate 3 true) [true true true]))
  )
)

(deftest ut-scalarproduct
  (testing
    (is (= (scalarproduct [1 2 3] [4 5 6]) 32))
  )
)

(deftest ut-myreverse
  (testing
    (is (= (myreverse [1 2 3]) [3 2 1]))
  )
)

(deftest ut-insert
  (testing
    (is (= (myinsert 3 [1 2 4 5]) [1 2 3 4 5]))
  )
)

(deftest ut-isort
  (testing
    (is (= (isort [2 5 1 3 4]) [1 2 3 4 5]))
    (is (= (isort [1]) [1]))
    (is (= (isort []) []))
  )
)

(deftest ut-mydrop
  (testing
    (is (= (mydrop 2 [1 2 3 4 5]) [3 4 5]))
    (is (= (mydrop 0 [1 2 3 4 5]) [1 2 3 4 5]))
    (is (= (mydrop 5 [1 2 3 4 5]) []))
    (is (= (mydrop 100 [1 2 3 4 5]) []))
  )
)

(deftest ut-myzip
  (testing
    (is (= (myzip [1 2 3 4 5] "abcde") [[1 \a] [2 \b] [3 \c] [4 \d] [5 \e]]))
    (is (= (myzip [1 2 3] "abcde") [[1 \a] [2 \b] [3 \c]]))
    (is (= (myzip [1 2 3 4 5] "abc") [[1 \a] [2 \b] [3 \c]]))
  )
)

(deftest ut-myeven?
  (testing
    (is (= (myeven? -1) false))
    (is (= (myeven? -2) true))
    (is (= (myeven? -3) false))
    (is (= (myeven? 0) true))
    (is (= (myeven? 1) false))
    (is (= (myeven? 2) true))
    (is (= (myeven? 3) false))
  )
)

(deftest ut-myodd?
  (testing
    (is (= (myodd? -1) true))
    (is (= (myodd? -2) false))
    (is (= (myodd? -3) true))
    (is (= (myodd? 0) false))
    (is (= (myodd? 1) true))
    (is (= (myodd? 2) false))
    (is (= (myodd? 3) true))
  )
)

(deftest ut-fibonacci
  (testing
    (is (= (fibonacci 0) 0))
    (is (= (fibonacci 1) 1))
    (is (= (fibonacci 2) 1))
    (is (= (fibonacci 3) 2))
    (is (= (fibonacci 4) 3))
  )
)

(deftest ut-qsort
  (testing
    (is (= (qsort [1 2 3]) [1 2 3]))
    (is (= (qsort [2 3 1]) [1 2 3]))
    (is (= (qsort [3 1 2]) [1 2 3]))
    (is (= (qsort [1]) [1]))
    (is (= (qsort []) []))
  )
)

(deftest ut-evens
  (testing
    (is (= (evens []) []))
    (is (= (evens [1 2]) [2]))
    (is (= (evens [1 2 3]) [2]))
    (is (= (evens [1 2 3 4]) [2 4]))
  )
)

(deftest ut-odds
  (testing
    (is (= (odds []) []))
    (is (= (odds [1 2]) [1]))
    (is (= (odds [1 2 3]) [1 3]))
    (is (= (odds [1 2 3 4]) [1 3]))
  )
)

(deftest ut-my-init
  (testing
    (is (= (my-init []) []))
    (is (= (my-init [1]) []))
    (is (= (my-init [1 2]) [1]))
    (is (= (my-init [1 2 3]) [1 2]))
  )
)

(deftest ut-elem
  (testing
    (is (not (elem 1 [])))
    (is (elem 1 [1]))
    (is (not (elem 3 [1 2])))
    (is (elem 3 [1 2 3]))
  )
)

(deftest ut-index
  (testing
    (is (= (my-index [1] 0) 1))
    (is (= (my-index [1 2] 1) 2))
  )
)

(deftest ut-my-merge
  (testing
    (is (= (my-merge [2 5 6] [1 3 4]) [1 2 3 4 5 6]))
  )
)

(deftest ut-msort
  (testing
    (is (= (msort []) []))
    (is (= (msort [1 2 3]) [1 2 3]))
    (is (= (msort [3 2 1]) [1 2 3]))
    (is (= (msort [2 3 1]) [1 2 3]))
  )
)

(deftest ut-my-replicate-rec
  (testing
    (is (= (my-replicate-rec 0 "a") []))
    (is (= (my-replicate-rec 1 "a") ["a"]))
    (is (= (my-replicate-rec 2 "a") ["a" "a"]))
  )
)

(deftest ut-my
  (testing
    (is (= (my 2 3) 8))
    (is (= (my 1 3) 1))
    (is (= (my 0 0) 1))
  )
)

(deftest ut-map
  (testing
    (is (= (my-map inc [1 2 3]) [2 3 4]))
    (is (= (my-map #(* % %) [1 2 3]) [1 4 9]))
  )
)

(deftest ut-filter
  (testing
    (is (= (my-filter odd? [1 2 3]) [1 3]))
    (is (= (my-filter odd? [2]) []))
    (is (= (my-filter odd? []) []))
  )
)

(deftest ut-map-recur
  (testing
    (is (= (my-map-recur inc [1 2 3]) [2 3 4]))
    (is (= (my-map-recur #(* % %) [1 2 3]) [1 4 9]))
  )
)

(deftest ut-all
  (testing
    (is (= (my-all odd? [1 3 5]) true))
    (is (= (my-all odd? [1 2 5]) false))
    (is (= (my-all odd? [2 3 5]) false))
    (is (= (my-all odd? [1 3 4]) false))
    (is (= (my-all odd? []) true))
  )
)

(deftest ut-any
  (testing
    (is (= (my-any odd? [1 3 5]) true))
    (is (= (my-any odd? [2 4 6]) false))
    (is (= (my-any odd? []) false))
  )
)

(deftest ut-my-drop-while
  (testing
    (is (= (my-drop-while odd? [1 2 3]) [2 3]))
    (is (= (my-drop-while odd? []) []))
    (is (= (my-drop-while odd? [2 3 4]) [2 3 4]))
  )
)

(deftest ut-my-filter-recur
  (testing
    (is (= (my-filter-recur odd? [1 2 3]) [1 3]))
    (is (= (my-filter-recur odd? []) []))
    (is (= (my-filter-recur odd? [2 4]) []))
  )
)

(deftest ut-my-take-while
  (testing
    (is (= (my-take-while odd? [1 2 3]) [1]))
    (is (= (my-take-while odd? [1 3 4]) [1 3]))
    (is (= (my-take-while odd? [1 3 5]) [1 3 5]))
    (is (= (my-take-while odd? []) []))
    (is (= (my-take-while odd? [2 3]) []))
  )
)

(deftest ut-my-foldr
  (testing
    (is (= (my-foldr + 0 [1 2 3 4]) 10))
    (is (= (apply str (my-foldr concat "Q" ["a" "b" "c" "d"])) "abcdQ"))
  )
)

(deftest ut-bit2int
  (testing
    (is (== (bit2int [0]) 0))
    (is (== (bit2int [1]) 1))
    (is (== (bit2int [0 1]) 2))
    (is (== (bit2int [1 1]) 3))
    (is (== (bit2int [0 0 1]) 4))
    (is (== (bit2int [1 0 1]) 5))
    (is (== (bit2int [0 1 1]) 6))
    (is (== (bit2int [1 1 1]) 7))
    (is (== (bit2int [0 0 0 1]) 8))
    (is (== (bit2int [1 0 0 1]) 9))
  )
)

(deftest ut-int2bit
  (testing
    (is (= (int2bit 1) [1]))
    (is (= (int2bit 2) [0 1]))
    (is (= (int2bit 3) [1 1]))
    (is (= (int2bit 4) [0 0 1]))
    (is (= (int2bit 5) [1 0 1]))
    (is (= (int2bit 6) [0 1 1]))
    (is (= (int2bit 7) [1 1 1]))
    (is (= (int2bit 8) [0 0 0 1]))
    (is (= (int2bit 9) [1 0 0 1]))
    (is (= (int2bit 10) [0 1 0 1]))
  )
)

(deftest ut-make8
  (testing
    (is (= (make8 [1]) [1 0 0 0 0 0 0 0]))
    (is (= (make8 [1 0 0 0 0 0 0 0 0]) [1 0 0 0 0 0 0 0]))
    (is (= (make8 [1 0 1]) [1 0 1 0 0 0 0 0]))
  )
)

(deftest ut-chop8
  (testing
    (is (= (chop8 [1 0 0 0 0 0 0 0 0]) [[1 0 0 0 0 0 0 0] [0 0 0 0 0 0 0 0]]))
    (is (= (chop8 [1 0 0 0 0 0 0 0 1]) [[1 0 0 0 0 0 0 0] [1 0 0 0 0 0 0 0]]))
  )
)

(deftest ut-decode
  (testing
    (is (= (decode [1 0 0 0 0 1 1 0 0 1 0 0 0 1 1 0]) "ab"))
    (is (= (decode [1 0 0 0 0 1 1 0 1 1 0 0 0 1 1 0]) "ac"))
  )
)

(deftest ut-encode
  (testing
    (is (= (encode "ab") [1 0 0 0 0 1 1 0 0 1 0 0 0 1 1 0]))
    (is (= (encode "ac") [1 0 0 0 0 1 1 0 1 1 0 0 0 1 1 0]))
  )
)

(deftest ut-my-all
  (testing
    (is (= (my-all #(= 1 %) [5,4,3,2,1]) false))
    (is (= (my-all #(= 1 %) [1,1,1]) true))
    (is (= (my-all #(= 1 %) []) true))
  )
)

(deftest ut-my-any
  (testing
    (is (= (my-any #(= 1 %) [5,4,3,2,1]) true))
    (is (= (my-any #(= 1 %) [1,1,1]) true))
    (is (= (my-any #(= 1 %) []) false))
  )
)

(deftest ut-my-drop-while
  (testing
    (is (= (my-drop-while odd? [1 2 3]) [2 3]))
    (is (= (my-drop-while odd? [1 3 4]) [4]))
    (is (= (my-drop-while odd? [1 3]) []))
  )
)

(deftest ut-my-take-while
  (testing
    (is (= (my-take-while odd? [1 2 3]) [1]))
    (is (= (my-take-while odd? [1 3 4]) [1 3]))
    (is (= (my-take-while odd? [2 3]) []))
  )
)

(deftest ut-crack
  (testing
    (is (= (crack (shift-string "i have attached work book, where we need to show two sheets in a dashboard." 3)) "i have attached work book, where we need to show two sheets in a dashboard."))
    (is (= (crack (shift-string "i have attached work book, where we need to show two sheets in a dashboard." 13)) "i have attached work book, where we need to show two sheets in a dashboard."))
    (is (= (crack (shift-string "i have attached work book, where we need to show two sheets in a dashboard." 23)) "i have attached work book, where we need to show two sheets in a dashboard."))
  )
)

(deftest ut-recently-modified?
  (testing
    (is (=
          (let [f (java.io.File. "./resources/ut-recently-modified.txt")]
            (.setLastModified f (- (System/currentTimeMillis) (* 1000 60 29)))
            (recently-modified? f))
          true))
    (is (=
          (let [f (java.io.File. "./resources/ut-recently-modified.txt")]
            (.setLastModified f (- (System/currentTimeMillis) (* 1000 60 31)))
            (recently-modified? f))
          false))
  )
)

(deftest ut-set
  (testing
    (is (=
      (set1)
      #{{:title "The Art of the Fugue", :composer "J. S. Bach"}
        {:title "Requiem", :composer "W. A. Mozart"}
        {:title "Requiem", :composer "Giuseppe Verdi"}
        {:title "Musical Offering", :composer "J. S. Bach"}}))
    (is (=
      (set2)
      #{{:name "Requiem", :composer "W. A. Mozart"}
        {:name "Requiem", :composer "Giuseppe Verdi"}}))
    (is (=
      (set3)
      #{{:name "Requiem"}
        {:name "The Art of the Fugue"}
        {:name "Musical Offering"}}))
    (is (=
      (set4)
      #{{:country "Italy", :name "Requiem", :composer "Giuseppe Verdi"}
        {:country "Germany", :name "Musical Offering", :composer "J. S. Bach"}
        {:country "Austria", :name "Requiem", :composer "W. A. Mozart"}
        {:country "Germany", :name "The Art of the Fugue", :composer "J. S. Bach"}}))
    (is (=
      (set5)
      #{{:country "Germany", :nation "Germany", :language "Germany", :composer "J. S. Bach"}
        {:country "Italy", :nation "Italy", :language "Italian", :composer "Giuseppe Verdi"}
        {:country "Austria", :nation "Austria", :language "German", :composer "W. A. Mozart"}}))
    (is (=
      (set6)
      #{{:country "Italy"}
        {:country "Austria"}}))
  )
)

(deftest ut-deeply-nested
  (testing
    (is (= (deeply-nested 0) 'bottom))
    (is (= (deeply-nested 1) '(bottom)))
    (is (= (deeply-nested 2) '((bottom))))
  )
)

(deftest ut-count-heads-pairs
  (testing
    (is (= (count-heads-pairs []) 0))
    (is (= (count-heads-pairs [:h]) 0))
    (is (= (count-heads-pairs [:t :h]) 0))
    (is (= (count-heads-pairs [:h :t]) 0))
    (is (= (count-heads-pairs [:t :t]) 0))
    (is (= (count-heads-pairs [:t :t :t]) 0))
    (is (= (count-heads-pairs [:h :t :t]) 0))
    (is (= (count-heads-pairs [:t :h :t]) 0))
    (is (= (count-heads-pairs [:t :t :h]) 0))
    (is (= (count-heads-pairs [:h :h]) 1))
    (is (= (count-heads-pairs [:h :h :h]) 2))
    (is (= (count-heads-pairs [:h :h :h :h]) 3))
    (is (= (count-heads-pairs [:h :h :t :h :h]) 2))
    (is (= (count-heads-pairs [:t :h :h :h :h]) 3))
    (is (= (count-heads-pairs [:h :h :h :h :t]) 3))
    (is (= (count-heads-pairs [:h :h :h :t :h]) 2))
    (is (= (count-heads-pairs [:h :t :t :h :h :h]) 2))
  )
)

(deftest ut-by-pairs
  (testing
    (is (= (by-pairs []) '()))
    (is (= (by-pairs [:t]) '()))
    (is (= (by-pairs [:t :t]) '((:t :t))))
    (is (= (by-pairs [:t :t :t]) '((:t :t) (:t :t))))
    (is (= (by-pairs [:t :t :t :t]) '((:t :t) (:t :t) (:t :t))))
    (is (= (by-pairs [:h :t :t :h :h :h]) '((:h :t) (:t :t) (:t :h) (:h :h) (:h :h))))
  )
)

(deftest ut-f
  (testing
    (is (< (elapsed-time (nth f-seq 250)) 100))
  )
)

(deftest ut-m
  (testing
    (is (< (elapsed-time (nth m-seq 250)) 100))
  )
)

(deftest ut-replace-symbol
  (is (= (replace-symbol [] 'a 'b') []))
  (is (= (replace-symbol ['a] 'a 'b) ['b]))
  (is (= (replace-symbol ['a 'b] 'a 'b) ['b 'b]))
  (is (= (replace-symbol ['b 'a] 'a 'b) ['b 'b]))
  (is (= (replace-symbol [['a] 'b] 'a 'b) [['b] 'b]))
  (is (= (replace-symbol ['b ['a]] 'a 'b) ['b ['b]]))
  (is (= (replace-symbol ['a ['a] 'b] 'a 'b) ['b ['b] 'b]))
  (is (= (replace-symbol [['a] [['a]] 'b] 'a 'b) [['b] [['b]] 'b]))
  (is
    (=
      (replace-symbol '((a b) (((b g r) (f r)) c (d e)) b) 'b 'a)
      '((a a) (((a g r) (f r)) c (d e)) a)
    )
  )
)

(deftest ut-make-greeter
  (testing
    (is (= ((make-greeter "Hello") "Yuji") "Hello, Yuji"))
    (is (= ((make-greeter "Aloha") "Yuji") "Aloha, Yuji"))
  )
)

; (deftest ut-recur-fibo
;   (testing
;     (is (= (recur-fibo 0) 0))
;     (is (= (recur-fibo 1) 1))
;     (is (= (recur-fibo 2) 1))
;     (is (= (recur-fibo 3) 2))
;     (is (= (recur-fibo 4) 3))
;     (is (= (recur-fibo 5) 5))
;     (is (= (recur-fibo 6) 8))
;     (is (> (recur-fibo 1000000) 1)) ; Not StackOverflowError
;   )
; )

; (deftest ut-lazy-seq-fibo
;   (is (= (take 10 (lazy-seq-fibo)) [0 1 1 2 3 5 8 13 21 34]))
;   (is (= (rem (nth (lazy-seq-fibo) 1000000) 1000) 875N))
; )

; (deftest ut-clojure-loc
;   (is (= (clojure-loc (java.io.File. "C:/Dropbox/_training/clojure-master/src/clj/clojure")) 17082))
;   (is (= (clojure-loc (java.io.File. "C:/Dropbox/_training/clojure-master/src/clj/clojure/core.clj")) 6576))
; )

(deftest ut-index-filter
  (is (= (index-filter #{\a \b} "abcdbbb") [0 1 4 5 6]))
  (is (= (index-filter #{\a \b} "xyz") []))
)

(deftest ut-get-composer
  (is (= (get-composer "./resources/compositions.xml") ["J. S. Bach" "J. S. Bach" "W. A. Mozart"]))
)

(deftest ut-my-and
  (is (my-and))
  (is (my-and true))
  (is (my-and true true))
  (is (not (my-and false)))
  (is (not (my-and true false)))
  (is (not (my-and false true)))
)

(deftest ut-my-or
  (is (not (my-or)))
  (is (not (my-or false)))
  (is (not (my-or false false)))
  (is (my-or true))
  (is (my-or true false))
  (is (my-or false true))
)

(deftest ut-my-odd?
  (is (= (my-odd? 0) false))
  (is (= (my-odd? 1) true))
  (is (= (my-odd? 2) false))
  (is (= (my-odd? 3) true))
  ; (is (= (my-odd? 100000) false))
)

(deftest ut-my-even?
  (is (= (my-even? 0) true))
  (is (= (my-even? 1) false))
  (is (= (my-even? 2) true))
  (is (= (my-even? 3) false))
  (is (= (my-even? 1000) true))
)

(deftest ut-list-files
  (is (= (list-files "./resources/ut-list-files/00") []))
  (is (= (set (list-files "./resources/ut-list-files/01")) #{"1" "a" "あ" "亜"}))
  (is (= (set (list-files "./resources/ut-list-files/02")) #{"dir01" "file01"}))
  (is (= (set (list-files "./resources/ut-list-files/03")) #{"dir_content_file01"}))
)

(deftest ut-chain
  (is (= (chain " abc " trim) "abc"))
  (is (= (chain " abc " trim length) 3))
)

(deftest ut-tail-fibo
  (is (= (tail-fibo 0) 0))
  (is (= (tail-fibo 1) 1))
  (is (= (tail-fibo 2) 1))
  (is (= (tail-fibo 3) 2))
  (is (= (tail-fibo 4) 3))
  (is (= (tail-fibo 5) 5))
  (is (= (tail-fibo 6) 8))
  (is (= (tail-fibo 7) 13))
)

(deftest ut-my-with-out-str
  (is (= (my-with-out-str (print "hello, ") (print "world!")) "hello, world!"))
)

(deftest ut-count-not-empty-line
  (is (= (count-not-empty-line (java.io.File. "./resources/ut-count-not-empty-line/01.txt")) 3))
  (is (= (count-not-empty-line (java.io.File. "./resources/ut-count-not-empty-line/02.txt")) 0))
  (is (= (count-not-empty-line (java.io.File. "./resources/ut-count-not-empty-line/03.txt")) 0))
  (is (= (count-not-empty-line (java.io.File. "./resources/ut-count-not-empty-line/04.txt")) 0))
  (is (= (count-not-empty-line (java.io.File. "./resources/ut-count-not-empty-line/05.txt")) 1))
  (is (= (count-not-empty-line (java.io.File. "./resources/ut-count-not-empty-line/06.txt")) 1))
  (is (= (count-not-empty-line (java.io.File. "./resources/ut-count-not-empty-line/07.txt")) 1))
  (is (= (count-not-empty-line (java.io.File. "./resources/ut-count-not-empty-line/08.txt")) 1))
)

(deftest ut-count-runs
  (is (= (count-runs 2 #(= :h %) [:h :t :t :h :h :h]) 2))
  (is (= (count-runs 2 #(= :t %) [:h :t :t :h :h :h]) 1))
  (is (= (count-runs 3 #(= :h %) [:h :t :t :h :h :h]) 1))
)

(deftest ut-fibo
  (is (= (take 10 (fibo)) '(0 1 1 2 3 5 8 13 21 34)))
  ; (is (= (rem (nth (fibo) 1000000) 1000) 875N))
)

(deftest ut-stack-consuming-fibo
  (is (= (stack-consuming-fibo 0) 0))
  (is (= (stack-consuming-fibo 1) 1))
  (is (= (stack-consuming-fibo 2) 1))
  (is (= (stack-consuming-fibo 3) 2))
  (is (= (stack-consuming-fibo 4) 3))
  (is (= (stack-consuming-fibo 5) 5))
  (is (= (stack-consuming-fibo 6) 8))
)

(deftest ut-blank?
  (is (= (blank? " ") true))
  (is (= (blank? "a") false))
  (is (= (blank? "") true))
  (is (= (blank? " a") false))
  (is (= (blank? "a ") false))
  (is (= (blank? " a ") false))
  (is (= (blank? "　") true))
  (is (= (blank? "  ") true))
)

(deftest ut-bench
  ; (is (= (bench (str "a" "b")) {:result "ab", :elapsed 53026}))
)

(deftest ut-p22
  (is (= (p22 '(1 2 3 3 1)) 5))
  (is (= (p22 "Hello World") 11))
  (is (= (p22 [[1 2] [3 4] [5 6]]) 3))
  (is (= (p22 '(13)) 1))
  (is (= (p22 '(:a :b :c)) 3))
)

(deftest ut-p23
  (is (= (p23 [1 2 3 4 5]) [5 4 3 2 1]))
  (is (= (p23 (sorted-set 5 7 2 7)) '(7 5 2)))
  (is (= (p23 [[1 2][3 4][5 6]]) [[5 6][3 4][1 2]]))
)

(deftest ut-p26
  (is (= (p26 3) '(1 1 2)))
  (is (= (p26 6) '(1 1 2 3 5 8)))
  (is (= (p26 8) '(1 1 2 3 5 8 13 21)))
)

(deftest ut-p27
  (is (false? (p27 '(1 2 3 4 5))))
  (is (true? (p27 "racecar")))
  (is (true? (p27 [:foo :bar :foo])))
  (is (true? (p27 '(1 1 3 3 1 1))))
  (is (false? (p27 '(:a :b :c))))
)

(deftest ut-p28
  (is (= (p28 '((1 2) 3 [4 [5 6]])) '(1 2 3 4 5 6)))
  (is (= (p28 ["a" ["b"] "c"]) '("a" "b" "c")))
  (is (= (p28 '((((:a))))) '(:a)))
)

(deftest ut-p30
  (is (= (apply str (p30 "Leeeeeerrroyyy")) "Leroy"))
  (is (= (p30 [1 1 2 3 3 2 2 3]) '(1 2 3 2 3)))
  (is (= (p30 [[1 2] [1 2] [3 4] [1 2]]) '([1 2] [3 4] [1 2])))
)

(deftest ut-p31
  (is (= (p31 [1 1 2 1 1 1 3 3]) '((1 1) (2) (1 1 1) (3 3))))
  (is (= (p31 [:a :a :b :b :c]) '((:a :a) (:b :b) (:c))))
  (is (= (p31 [[1 2] [1 2] [3 4]]) '(([1 2] [1 2]) ([3 4]))))
)

(deftest ut-p32
	(is (= (p32 [1 2 3]) '(1 1 2 2 3 3)))
	(is (= (p32 [:a :a :b :b]) '(:a :a :a :a :b :b :b :b)))
	(is (= (p32 [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4])))
	(is (= (p32 [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4])))
)

(deftest ut-p33
	(is  (= (p33  [1 2 3] 2) '(1 1 2 2 3 3)))
	(is  (= (p33  [:a :b] 4) '(:a :a :a :a :b :b :b :b)))
	(is  (= (p33  [4 5 6] 1) '(4 5 6)))
	(is  (= (p33  [[1 2] [3 4]] 2) '([1 2] [1 2] [3 4] [3 4])))
	(is  (= (p33  [44 33] 2) [44 44 33 33]))
)

(deftest ut-p34
	(is  (= (p34 1 4) '(1 2 3)))
	(is  (= (p34 -2 2) '(-2 -1 0 1)))
	(is  (= (p34 5 8) '(5 6 7)))
)

(deftest ut-p38
  (is (= (p38 1 8 3 4) 8))
  (is (= (p38 30 20) 30))
  (is (= (p38 45 67 11) 67))
)

(deftest ut-p39
	(is (= (p39 [1 2 3] [:a :b :c]) '(1 :a 2 :b 3 :c)))
	(is (= (p39 [1 2] [3 4 5 6]) '(1 3 2 4)))
	(is (= (p39 [1 2 3 4] [5]) [1 5]))
	(is (= (p39 [30 20] [25 15]) [30 25 20 15]))
)

(deftest ut-p40
	(is (= (p40 0 [1 2 3]) [1 0 2 0 3]))
	(is (= (apply str (p40 ", " ["one" "two" "three"])) "one, two, three"))
	(is (= (p40 :z [:a :b :c :d]) [:a :z :b :z :c :z :d]))
)

(deftest ut-p41
	(is (= (p41 [1 2 3 4 5 6 7 8] 3) [1 2 4 5 7 8]))
	(is (= (p41 [:a :b :c :d :e :f] 2) [:a :c :e]))
	(is (= (p41 [1 2 3 4 5 6] 4) [1 2 3 5 6]))
)

(deftest ut-p42
	(is (= (p42 1) 1))
	(is (= (p42 3) 6))
	(is (= (p42 5) 120))
	(is (= (p42 8) 40320))
)

(deftest ut-p43
	(is (= (p43 [1 2 3 4 5 6] 2) '((1 3 5) (2 4 6))))
	(is (= (p43 (range 9) 3) '((0 3 6) (1 4 7) (2 5 8))))
	(is (= (p43 (range 10) 5) '((0 5) (1 6) (2 7) (3 8) (4 9))))
)

(deftest ut-p44
	(is (= (p44 2 [1 2 3 4 5]) '(3 4 5 1 2)))
	(is (= (p44 -2 [1 2 3 4 5]) '(4 5 1 2 3)))
	(is (= (p44 6 [1 2 3 4 5]) '(2 3 4 5 1)))
	(is (= (p44 1 '(:a :b :c)) '(:b :c :a)))
	(is (= (p44 -4 '(:a :b :c)) '(:c :a :b)))
)

(deftest ut-p50
	(is (= (set (p50 [1 :a 2 :b 3 :c])) #{[1 2 3] [:a :b :c]}))
	(is (= (set (p50 [:a "foo"  "bar" :b])) #{[:a :b] ["foo" "bar"]}))
	(is (= (set (p50 [[1 2] :a [3 4] 5 6 :b])) #{[[1 2] [3 4]] [:a :b] [5 6]}))
)

(deftest ut-p53
	(is (= (p53 [1 0 1 2 3 0 4 5]) [0 1 2 3]))
	(is (= (p53 [5 6 1 3 2 7]) [5 6]))
	(is (= (p53 [2 3 3 4 5]) [3 4 5]))
	(is (= (p53 [7 6 5 4]) []))
)

(deftest ut-p54
	(is (= (p54 3 (range 9)) '((0 1 2) (3 4 5) (6 7 8))))
	(is (= (p54 2 (range 8)) '((0 1) (2 3) (4 5) (6 7))))
	(is (= (p54 3 (range 8)) '((0 1 2) (3 4 5))))
)

(deftest ut-p55
	(is (= (p55 [1 1 2 3 2 1 1]) {1 4, 2 2, 3 1}))
	(is (= (p55 [:b :a :b :a :b]) {:a 2, :b 3}))
	(is (= (p55 '([1 2] [1 3] [1 3])) {[1 2] 1, [1 3] 2}))
)

(deftest ut-p56
	(is (= (p56 [1 2 1 3 1 2 4]) [1 2 3 4]))
	(is (= (p56 [:a :a :b :b :c :c]) [:a :b :c]))
	(is (= (p56 '([2 4] [1 2] [1 3] [1 3])) '([2 4] [1 2] [1 3])))
	(is (= (p56 (range 50)) (range 50)))
)

(deftest ut-p58
	(is (= [3 2 1] ((p58 rest reverse) [1 2 3 4])))
	(is (= 5 ((p58 (partial + 3) second) [1 2 3 4])))
	(is (= true ((p58 zero? #(mod % 8) +) 3 5 7 9)))
	(is (= "HELLO" ((p58 #(.toUpperCase %) #(apply str %) take) 5 "hello world")))
)

(deftest ut-p59
	(is (= [21 6 1] ((p59 + max min) 2 3 5 1 6 4)))
	(is (= ["HELLO" 5] ((p59 #(.toUpperCase %) count) "hello")))
	(is (= [2 6 4] ((p59 :a :c :b) {:a 2, :b 4, :c 6, :d 8 :e 10})))
)

(deftest ut-p60
	(is (= (take 5 (p60 + (range))) [0 1 3 6 10]))
	(is (= (p60 conj [1] [2 3 4]) [[1] [1 2] [1 2 3] [1 2 3 4]]))
	(is (= (last (p60 * 2 [3 4 5])) (reduce * 2 [3 4 5]) 120))
)

(deftest ut-p67
	(is (= (p67 2) [2 3]))
	(is (= (p67 5) [2 3 5 7 11]))
	(is (= (last (p67 100)) 541))
)

(deftest ut-p69
  (is (= (p69 * {:a 2, :b 3, :c 4} {:a 2} {:b 2} {:c 5})
    {:a 4, :b 6, :c 20}))
  (is (= (p69 - {1 10, 2 20} {1 3, 2 10, 3 15})
    {1 7, 2 10, 3 15}))
  (is (= (p69 concat {:a [3], :b [6]} {:a [4 5], :c [8 9]} {:b [7]})
    {:a [3 4 5], :b [6 7], :c [8 9]}))
)

(deftest ut-p74
  (is (= (p74 "4,5,6,7,8,9") "4,9"))
  (is (= (p74 "15,16,25,36,37") "16,25,36"))
)

(deftest ut-p75
	(is (= (p75 1) 1))
	(is (= (p75 10) (count '(1 3 7 9)) 4))
	(is (= (p75 40) 16))
	(is (= (p75 99) 60))
)

(deftest ut-p77
  (is (= (p77 ["meat" "mat" "team" "mate" "eat"])
     #{#{"meat" "team" "mate"}}))
  (is (= (p77 ["veer" "lake" "item" "kale" "mite" "ever"])
     #{#{"veer" "ever"} #{"lake" "kale"} #{"mite" "item"}}))
)

(deftest ut-p78
  (is (= (letfn [(triple [x] #(sub-two (* 3 x)))
            (sub-two [x] #(stop?(- x 2)))
            (stop? [x] (if (> x 50) x #(triple x)))]
      (p78 triple 2))
    82))
  (is (= (letfn [(my-even? [x] (if (zero? x) true #(my-odd? (dec x))))
            (my-odd? [x] (if (zero? x) false #(my-even? (dec x))))]
      (map (partial p78 my-even?) (range 6)))
    [true false true false true false]))
)

(deftest ut-p80
	(is (= (p80 6) true))
	(is (= (p80 7) false))
	(is (= (p80 496) true))
	(is (= (p80 500) false))
	(is (= (p80 8128) true))
)

(deftest ut-p86
	(is (= (p86 7) true))
	(is (= (p86 986543210) true))
	(is (= (p86 2) false))
	(is (= (p86 3) false))
)

(deftest ut-p95
  (is (= (p95 '(:a (:b nil nil) nil))
     true))
  (is (= (p95 '(:a (:b nil nil)))
     false))
  (is (= (p95 [1 nil [2 [3 nil nil] [4 nil nil]]])
     true))
  (is (= (p95 [1 [2 nil nil] [3 nil nil] [4 nil nil]])
     false))
  (is (= (p95 [1 [2 [3 [4 nil nil] nil] nil] nil])
     true))
  (is (= (p95 [1 [2 [3 [4 false nil] nil] nil] nil])
     false))
  (is (= (p95 '(:a nil ()))
     false))
)

(deftest ut-p96
	(is (= (p96 '(:a (:b nil nil) (:b nil nil))) true))
	(is (= (p96 '(:a (:b nil nil) nil)) false))
	(is (= (p96 '(:a (:b nil nil) (:c nil nil))) false))
	(is (= (p96 [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
	               [2 [3 nil [4 [6 nil nil] [5 nil nil]]] nil]])
	       true))
	(is (= (p96 [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
	               [2 [3 nil [4 [5 nil nil] [6 nil nil]]] nil]])
	       false))
	(is (= (p96 [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
	            [2 [3 nil [4 [6 nil nil] nil]] nil]])
         false))
)

(deftest ut-p97
  (is (= (p97 1) [1]))
  (is (= (map p97 (range 1 6))
     [     [1]
          [1 1]
         [1 2 1]
        [1 3 3 1]
       [1 4 6 4 1]]))
  (is (= (p97 11)
     [1 10 45 120 210 252 210 120 45 10 1]))
)

(deftest ut-p98
  (is (= (p98 #(* % %) #{-2 -1 0 1 2})
        #{#{0} #{1 -1} #{2 -2}}))
  (is (= (p98 #(rem % 3) #{0 1 2 3 4 5 })
        #{#{0 3} #{1 4} #{2 5}}))
  (is (= (p98 identity #{0 1 2 3 4})
        #{#{0} #{1} #{2} #{3} #{4}}))
  (is (= (p98 (constantly true) #{0 1 2 3 4})
        #{#{0 1 2 3 4}}))
)

(deftest ut-p100
	(is (== (p100 2 3) 6))
	(is (== (p100 5 3 7) 105))
	(is (== (p100 1/3 2/5) 2))
	(is (== (p100 3/4 1/6) 3/2))
	(is (== (p100 7 5/7 2 3/5) 210))
)

(deftest ut-p102
	(is (= (p102 "something") "something"))
	(is (= (p102 "multi-word-key") "multiWordKey"))
	(is (= (p102 "leaveMeAlone") "leaveMeAlone"))
)

(deftest ut-p115
	(is (= true (p115 11)))
	(is (= true (p115 121)))
	(is (= false (p115 123)))
	(is (= true (p115 0)))
	(is (= false (p115 88099)))
	(is (= true (p115 89098)))
	(is (= true (p115 89089)))
	(is (= (take 20 (filter p115 (range)))
	       [0 1 2 3 4 5 6 7 8 9 11 22 33 44 55 66 77 88 99 101]))
)

(deftest ut-p135
	(is (= 7  (p135 2 + 5)))
	(is (= 42 (p135 38 + 48 - 2 / 2)))
	(is (= 8  (p135 10 / 2 - 1 * 2)))
	(is (= 72 (p135 20 / 2 + 2 + 4 + 8 - 6 - 10 * 9)))
)

(deftest ut-p146
  (is (= (p146 '{a {p 1, q 2}
                 b {m 3, n 4}})
        '{[a p] 1, [a q] 2
          [b m] 3, [b n] 4}))
  (is (= (p146 '{[1] {a b c d}
                 [2] {q r s t u v w x}})
        '{[[1] a] b, [[1] c] d,
          [[2] q] r, [[2] s] t,
          [[2] u] v, [[2] w] x}))
  (is (= (p146 '{m {1 [a b c] 3 nil}})
         '{[m 1] [a b c], [m 3] nil}))
)

(deftest ut-p147
	(is (= (second (p147 [2 3 2])) [2 5 5 2]))
	(is (= (take 5 (p147 [1])) [[1] [1 1] [1 2 1] [1 3 3 1] [1 4 6 4 1]]))
	(is (= (take 2 (p147 [3 1 2])) [[3 1 2] [3 4 3 2]]))
	(is (= (take 100 (p147 [2 4 2])) (rest (take 101 (p147 [2 2])))))
)

(deftest ut-p85
	(is (= (p85 #{1 :a}) #{#{1 :a} #{:a} #{} #{1}}))
	(is (= (p85 #{}) #{#{}}))
	(is (= (p85 #{1 2 3})
   #{#{} #{1} #{2} #{3} #{1 2} #{1 3} #{2 3} #{1 2 3}}))
	(is (= (count (p85 (into #{} (range 10)))) 1024))
)

(deftest ut-p105
	(is (= {} (p105 [])))
	(is (= {:a [1]} (p105 [:a 1])))
	(is (= {:a [1], :b [2]} (p105 [:a 1, :b 2])))
	(is (= {:a [1 2 3], :b [], :c [4]} (p105 [:a 1 2 3 :b :c 4])))
)

(deftest ut-p137
	(is (= [1 2 3 4 5 0 1] (p137 1234501 10)))
	(is (= [0] (p137 0 11)))
	(is (= [1 0 0 1] (p137 9 2)))
	(is (= [1 0] (let [n (rand-int 100000)](p137 n n))))
	(is (= [16 18 5 24 15 1] (p137 Integer/MAX_VALUE 42)))
)

(deftest ut-p110
	(is (= [[1 1] [2 1] [1 2 1 1]] (take 3 (p110 [1]))))
	(is (= [3 1 2 4] (first (p110 [1 1 1 4 4]))))
	(is (= [1 1 1 3 2 1 3 2 1 1] (nth (p110 [1]) 6)))
	(is (= 338 (count (nth (p110 [3 2]) 15))))
)

(deftest ut-p144
	(is (= (take 3 (p144 3.14 int double)) [3.14 3 3.0]))
	(is (= (take 5 (p144 3 #(- % 3) #(+ 5 %))) [3 0 5 2 7]))
	(is (= (take 12 (p144 0 inc dec inc dec inc)) [0 1 0 1 0 1 2 1 2 1 2 3]))
)

(deftest ut-p108
	(is (= 3 (p108 [3 4 5])))
	(is (= 4 (p108 [1 2 3 4 5 6 7] [0.5 3/2 4 19])))
	(is (= 7 (p108 (range) (range 0 100 7/6) [2 3 5 7 11 13])))
	(is (= 64 (p108 (map #(* % % %) (range)) ;; perfect cubes
	                (filter #(zero? (bit-and % (dec %))) (range)) ;; powers of 2
                  (iterate inc 20)))) ;; at least as large as 20
)

(deftest ut-p93
  (is (= (p93 [["Do"] ["Nothing"]])
         [["Do"] ["Nothing"]]))
  (is (= (p93 [[[[:a :b]]] [[:c :d]] [:e :f]])
         [[:a :b] [:c :d] [:e :f]]))
  (is (= (p93 '((1 2)((3 4)((((5 6)))))))
         '((1 2)(3 4)(5 6))))
)

(deftest ut-p158
  (is (= 10 ((p158 (fn [a]
                     (fn [b]
                       (fn [c]
                         (fn [d]
                           (+ a b c d))))))
            1 2 3 4)))
  (is (= 24 ((p158 (fn [a]
                     (fn [b]
                       (fn [c]
                         (fn [d]
                           (* a b c d))))))
            1 2 3 4)))
  (is (= 25 ((p158 (fn [a]
                     (fn [b]
                       (* a b))))
            5 5)))
)

(deftest ut-p114
	(is (= [2 3 5 7 11 13]
         (p114 4 #(= 2 (mod % 3))
           [2 3 5 7 11 13 17 19 23])))
	(is (= ["this" "is" "a" "sentence"]
         (p114 3 #(some #{\i} %)
           ["this" "is" "a" "sentence" "i" "wrote"])))
	(is (= ["this" "is"]
         (p114 1 #{"a"}
           ["this" "is" "a" "sentence" "i" "wrote"])))
)

(deftest ut-p132
	(is (= '(1 :less 6 :less 7 4 3) (p132 < :less [1 6 7 4 3])))
	(is (= '(2) (p132 > :more [2])))
	(is (= [0 1 :x 2 :x 3 :x 4]  (p132 #(and (pos? %) (< % %2)) :x (range 5))))
  (is (empty? (p132 > :more ())))
	(is (= [0 1 :same 1 2 3 :same 5 8 13 :same 21]
   (take 12 (->> [0 1]
                 (iterate (fn [[a b]] [b (+ a b)]))
                 (map first) ; fibonacci numbers
                 (p132 (fn [a b] ; both even or both odd
                       (= (mod a 2) (mod b 2)))
                     :same)))))
)

(deftest ut-p104
	(is (= "I" (p104 1)))
	(is (= "XXX" (p104 30)))
	(is (= "IV" (p104 4)))
	(is (= "CXL" (p104 140)))
	(is (= "DCCCXXVII" (p104 827)))
	(is (= "MMMCMXCIX" (p104 3999)))
	(is (= "XLVIII" (p104 48)))
)

(deftest ut-p92
	(is (= 14 (p92 "XIV")))
	(is (= 827 (p92 "DCCCXXVII")))
	(is (= 3999 (p92 "MMMCMXCIX")))
	(is (= 48 (p92 "XLVIII")))
)

(deftest ut-p103
	(is (= (p103 1 #{4 5 6}) #{#{4} #{5} #{6}}))
  (is (= (p103 10 #{4 5 6}) #{}))
	(is (= (p103 2 #{0 1 2}) #{#{0 1} #{0 2} #{1 2}}))
	(is (= (p103 3 #{0 1 2 3 4}) #{#{0 1 2} #{0 1 3} #{0 1 4} #{0 2 3} #{0 2 4}
                         #{0 3 4} #{1 2 3} #{1 2 4} #{1 3 4} #{2 3 4}}))
	(is (= (p103 4 #{[1 2 3] :a "abc" "efg"}) #{#{[1 2 3] :a "abc" "efg"}}))
	(is (= (p103 2 #{[1 2 3] :a "abc" "efg"}) #{#{[1 2 3] :a} #{[1 2 3] "abc"} #{[1 2 3] "efg"}
                                    #{:a "abc"} #{:a "efg"} #{"abc" "efg"}}))
)

(deftest ut-p116
  (is (= false (p116 4)))
  (is (= true (p116 5)))
  (is (= true (p116 53)))
  (is (= true (p116 563)))
  (is (= true (p116 1103)))
  (is (= 1103 (nth (filter p116 (range)) 15)))
)
