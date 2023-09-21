(ns clj.core-test
  (:require [clojure.test :refer :all]
            [clj.core :refer :all]))

(deftest unique-attribute-test
  (testing
      (is (= 1 (unique-attribute [1 2 2])))
    (is (= 3 (unique-attribute [1 2 2 3 1])))
    (is (= 1 (unique-attribute [1])))))

(deftest move-zeros-2-back-test
  (testing
      (is (= [1,2,3,0] (move-zeros-2-back [0,1,2,3])))
    (is (= [4,1,5,0,0] (move-zeros-2-back [4,1,0,0,5])))))

(deftest merge-intervals-test
  (testing
      (is (= [[1,4]] (merge-intervals [[1,2],[2,3],[3,4]])))
    (is (= [[1,3],[4,5]] (merge-intervals [[1,3],[1,3],[4,5]])))
    (is (= [[0,3],[4,5]] (merge-intervals [[1,3],[0,3],[4,5]])))
    (is (= [[1,6]] (merge-intervals [[1,6],[2,5],[3,4]])))
    (is (= [[1,3],[4,6],[7,9]] (merge-intervals [[1,3],[4,6],[7,9]])))))

(deftest my-zip-test
  (testing
    (is (= '((1 \x \a) (2 \y \b)) (my-zip [1 2] "xy" "ab")))
    (is (= '((1 \x)) (my-zip [1 2] "x")))
    (is (= '((1 \x)) (my-zip [1] "xy")))
    (is (= nil (my-zip)))
    (is (or (= (my-zip "") nil) (= (my-zip "") '())))
    (is (or (= (my-zip "" [1]) nil) (= (my-zip "" [1]) '())))
    (is (or (= (my-zip [1] "") nil) (= (my-zip [1] "") '())))
    )
  )

(deftest sum-test
  (testing
      (is (= 1 (sum [1])))
    (is (= 3 (sum [1 2])))
    (is (= 0 (sum [])))
    )
  )

(deftest qsort01-test
  (testing
      (is (=  (qsort01 [3 2 1]) [1 2 3] ))
    (is (=  (qsort01 [3 2 3 1]) [1 2 3 3] ))
    (is (=  (qsort01 [2 3 1]) [1 2 3] ))
    (is (=  (qsort01 [1 2 3]) [1 2 3] ))
    (is (=  (qsort01 "cba") '(\a \b \c) ))
    (is (=  (qsort01 "bca") '(\a \b \c) ))
    (is (=  (qsort01 "abc") '(\a \b \c) ))
    (is (=  (qsort01 '(1)) '(1) ))
    (is (=  (qsort01 "a") '(\a) ))
    (is (=  (qsort01 "") () ))
    (is (=  (qsort01 ()) () ))
    )
  )

(deftest product-test
  (testing
      (is (= 6 (product [1 2 3])))
    (is (= 1 (product [1])))
    (is (= 1 (product [])))
    )
  )

(deftest rqsort-test
  (testing
      (is (=  [3 2 1] (rqsort [3 2 1])))
    (is (=  [3 2 1] (rqsort [2 3 1])))
    (is (=  [3 2 1] (rqsort [1 2 3])))
    (is (=  '(\c \b \a) (rqsort "cba")))
    (is (=  '(\c \b \a) (rqsort "bca")))
    (is (=  '(\c \b \a) (rqsort "abc")))
    (is (=  '(1) (rqsort '(1))))
    (is (=  '(\a) (rqsort "a")))
    (is (=  () (rqsort "")))
    (is (=  () (rqsort ())))
    )
  )

(deftest last-test
  (testing
      (is (=  3 (my-last [1 2 3])))
    (is (=  2 (my-last [1 2])))
    (is (=  1 (my-last [1])))
    (is (thrown? java.util.NoSuchElementException (my-last [])))
    )
  )

(deftest halve-test
  (testing
      (is (=  [[1] [2]] (halve [1 2])))
    (is (=  [[1 2] [3 4]] (halve [1 2 3 4])))
    (is (thrown? java.lang.IllegalArgumentException (halve [])))
    (is (thrown? java.lang.IllegalArgumentException (halve [1])))
    (is (thrown? java.lang.IllegalArgumentException (halve [1 2 3])))        
    )
  )

(deftest concat-test
  (testing
      (is (=  [1 2 3 4] (my-concat [[1 2] [3 4]])))
    (is (=  [1 2 3] (my-concat [[1 2] [3]])))
    (is (=  [1 2] (my-concat [[1 2]])))
    )
  )

(deftest my-factors-test
  (testing
      (is (=  [1 2 5 10] (my-factors 10)))
    (is (=  [1 5] (my-factors 5)))
    (is (=  [1] (my-factors 1)))
    (is (=  [] (my-factors 0)))
    )
  )

(deftest und
  (testing
      (is (=  [] (my-find 4 [ [1 \a] [2 \b] ])))
    (is (=  [\a] (my-find 1 [ [1 \a] [2 \b] ])))
    (is (=  [\a] (my-find 1 [ [2 \b] [1 \a] ])))
    (is (=  [\a] (my-find 1 [ [2 \b] [1 \a] [3 \c] ])))
    (is (=  [\a \b] (my-find 1 [ [1 \a] [1 \b] [3 \c] ])))
    (is (=  [1] (my-find \a [ [\a 1] [\b 2] [\c 3] ])))
    (is (=  [1] (my-find "abc" [ ["abc" 1] ["def" 2] ])))
    )
  )

(deftest firsts-test
  (testing
      (is (=  [1] (firsts [[1]])))
    (is (=  [1] (firsts [[1 2]])))
    (is (=  [1] (firsts [[1 2 3]])))
    (is (=  [1 2] (firsts [[1] [2]])))
    (is (=  [1 3] (firsts [[1 2] [3 4]])))
    (is (=  [1 4] (firsts [[1 2 3] [4 5 6]])))
    (is (=  [\a \d] (firsts ["abc" "def"])))
    )
  )

(deftest length-test
  (testing
      (is (=  0 (length [])))
    (is (=  1 (length [1])))
    (is (=  2 (length [1 2])))
    (is (=  3 (length "abc")))
    (is (=  3 (length #{1 2 3})))
    (is (=  3 (length {:a 1 :b 2 :c 3})))
    )
  )

(deftest prime-test
  (testing
      (is (not (prime? 1)))
    (is (prime? 2))
    (is (prime? 3))
    (is (not (prime? 4)))
    (is (prime? 5))
    (is (not (prime? 6)))
    )
  )

(deftest primes-test
  (testing
      (is (=  '() (primes 1)))
    (is (=  '(2 3 5) (primes 5)))
    )
  )

(deftest pairs-test
  (testing
      (is (=  {1 2} (pairs [1 2])))
    (is (=  {1 2, 2 3} (pairs [1 2 3])))
    (is (=  {1 2, 2 3, 3 4} (pairs [1 2 3 4])))
    (is (=  {} (pairs [1])))
    (is (=  {} (pairs [])))
    (is (=  {\a \b, \b \c} (pairs "abc")))
    )
  )

(deftest sorted-test
  (testing
      (is (sorted? [1 2]))
    (is (not (sorted? [2 1])))
    (is (sorted? [1]))
    (is (sorted? []))
    )
  )

(deftest positions-test
  (testing
      (is (=  [0] (positions 1 [1 2])))
    (is (=  [0 2] (positions 1 [1 2 1])))
    (is (=  [1 2] (positions 1 [0 1 1 2])))
    (is (=  [] (positions 1 [2 3])))
    )
  )

(deftest char-count-test
  (testing
      (is (=  1 (char-count \a "abc")))
    (is (=  1 (char-count \b "abc")))
    (is (=  1 (char-count \c "abc")))
    (is (=  1 (char-count \c "abca")))
    (is (=  1 (char-count \c "abbca")))
    (is (=  3 (char-count \c "abbccac")))
    )
  )

(deftest lowers-test
  (testing
      (is (=  3 (lower-count "abcあ亜ア")))
    (is (=  2 (lower-count "あ亜アAbc")))
    (is (=  2 (lower-count "aBcあ亜ア")))
    (is (=  2 (lower-count "あ亜アabC")))
    )
  )

(deftest int2let-test
  (testing
      (is (=  \a (int2let 0)))
    (is (=  \b (int2let 1)))
    (is (=  \c (int2let 2)))
    )
  )

(deftest let2int-test
  (testing
      (is (=  0 (let2int \a)))
    (is (=  1 (let2int \b)))
    (is (=  2 (let2int \c)))
    )
  )

(deftest shift-test
  (testing
      (is (=  \b (my-shift 1 \a)))
    (is (=  \a (my-shift 1 \z)))
    (is (=  \A (my-shift 1 \A)))
    )
  )

(deftest my-encode-test
  (testing
      (is (=  "bcd" (my-encode 1 "abc")))
    (is (=  "yza" (my-encode 1 "xyz")))
    )
  )

(deftest percent-test
  (testing
      (is (=  100 (percent 1 1)))
    (is (=  50N (percent 1 2)))
    (is (=  100/3 (percent 1 3)))
    )
  )

(deftest freqs-test
  (testing
      (is (=  [100 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] (freqs "a")))
    (is (=  [200/3 100/3 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] (freqs "aab")))
    (is (=  [50N 50N 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] (freqs "ab")))
    (is (=  [0 100/3 100/3 100/3 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] (freqs "bcd")))
    (is (=  [50N 0 50N 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] (freqs "ac")))
    (is (=  [50N 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 50N] (freqs "az")))    
    )
  )

(deftest chisqr-test
  (testing
      (is (= (chisqr [1 2 3] [1 2 3]) 0.0))
    (is (= (chisqr [1 2 3] [1 2]) 0.0))    
    (is (= (chisqr [13 7 5 15] [9 11 9 11]) 6.464646577835083))
    ))

(deftest rotate-test
  (testing
      (is (=  "bca" (rotate 1 "abc")))
    (is (=  "cab" (rotate -1 "abc")))
    (is (=  "cab" (rotate 2 "abc")))
    (is (=  "bca" (rotate -2 "abc")))
    (is (=  "abc" (rotate 3 "abc")))
    (is (=  "abc" (rotate -3 "abc")))
    (is (=  "bca" (rotate 4 "abc")))
    (is (=  "cab" (rotate -4 "abc")))
    )
  )

(deftest sum-square-1-to-100-test
  (testing
      (is (sum-square-1-to-100) 338350)
    )
  )

(deftest perfects-test
  (testing
      (is (=  [6] (perfects 10)))
    (is (=  [6 28] (perfects 100)))
    )
  )

(deftest pyths-test
  (testing
      (is (= 
           [[3 4 5] [6 8 10]]
           (pyths 10)))
    )
  )

(deftest my-replicate-test
  (testing
      (is (=  [true true true] (my-replicate 3 true)))
    )
  )

(deftest scalarproduct-test
  (testing
      (is (= (scalarproduct [1 2 3] [4 5 6]) 32))
    )
  )

(deftest my-reverse-test
  (testing
      (is (=  [3 2 1] (my-reverse [1 2 3])))
    )
  )

(deftest insert-test
  (testing
      (is (=  [1 2 3 4 5] (my-insert 3 [1 2 4 5])))
    (is (=  [1] (my-insert 1 [])))
    )
  )

(deftest isort-test
  (testing
      (is (=  [1 2 3 4 5] (isort [2 5 1 3 4])))
    (is (=  [1] (isort [1])))
    (is (=  [] (isort [])))
    )
  )

(deftest my-drop-test
  (testing
      (is (=  [3 4 5] (my-drop 2 [1 2 3 4 5])))
    (is (=  [1 2 3 4 5] (my-drop 0 [1 2 3 4 5])))
    (is (=  [] (my-drop 5 [1 2 3 4 5])))
    (is (=  [] (my-drop 100 [1 2 3 4 5])))
    (is (=  [] (my-drop 1 [])))
    )
  )

(deftest my-zip2-test
  (testing
      (is (=  [[1 \a] [2 \b] [3 \c] [4 \d] [5 \e]] (my-zip2 [1 2 3 4 5] "abcde")))
    (is (=  [[1 \a] [2 \b] [3 \c]] (my-zip2 [1 2 3] "abcde")))
    (is (=  [[1 \a] [2 \b] [3 \c]] (my-zip2 [1 2 3 4 5] "abc")))
    )
  )

(deftest my-even?-test
  (testing
      (is (=  false (my-even? -1)))
    (is (=  true (my-even? -2)))
    (is (=  false (my-even? -3)))
    (is (=  true (my-even? 0)))
    (is (=  false (my-even? 1)))
    (is (=  true (my-even? 2)))
    (is (=  false (my-even? 3)))
    ) 
 )

(deftest my-odd?-test
  (testing
      (is (=  true (my-odd? -1)))
    (is (=  false (my-odd? -2)))
    (is (=  true (my-odd? -3)))
    (is (=  false (my-odd? 0)))
    (is (=  true (my-odd? 1)))
    (is (=  false (my-odd? 2)))
    (is (=  true (my-odd? 3)))
    )
  )

(deftest fibonacci-test
  (testing
      (is (=  0 (fibonacci 0)))
    (is (=  1 (fibonacci 1)))
    (is (=  1 (fibonacci 2)))
    (is (=  2 (fibonacci 3)))
    (is (=  3 (fibonacci 4)))
    )
  )

(deftest qsort-test
  (testing
      (is (= (qsort [1 2 3]) [1 2 3]))
    (is (= (qsort [2 3 1]) [1 2 3]))
    (is (= (qsort [2 3 2 1]) [1 2 2 3]))
    (is (= (qsort [3 1 2]) [1 2 3]))
    (is (= (qsort [1]) [1]))
    (is (= (qsort []) []))
    )
  )

(deftest evens-test
  (testing
      (is (=  [] (evens [])))
    (is (=  [2] (evens [1 2])))
    (is (=  [2] (evens [1 2 3])))
    (is (=  [2 4] (evens [1 2 3 4])))
    )
  )

(deftest odds-test
  (testing
      (is (=  [] (odds [])))
    (is (=  [1] (odds [1 2])))
    (is (=  [1 3] (odds [1 2 3])))
    (is (=  [1 3] (odds [1 2 3 4])))
    )
  )

(deftest my-init-test
  (testing
      (is (=  [] (my-init [])))
    (is (=  [] (my-init [1])))
    (is (=  [1] (my-init [1 2])))
    (is (=  [1 2] (my-init [1 2 3])))
    )
  )

(deftest my-elem-test
  (testing
      (is (= false (my-elem 1 [])))
    (is (= true (my-elem 1 [1])))
    (is (= false (my-elem 3 [1 2])))
    (is (= true (my-elem 3 [1 2 3])))
    )
  )

(deftest my-index-test
  (testing
      (is (=  nil (my-index [] 0)))
    (is (=  nil (my-index [] 1)))
    (is (=  1 (my-index [1] 0)))
    (is (=  2 (my-index [1 2] 1)))
    )
  )

(deftest my-merge-test
  (testing
      (is (= (my-merge [2 5 6] [1 3 4]) [1 2 3 4 5 6]))
    (is (= (my-merge [] [1 2]) [1 2]))
    (is (= (my-merge [1 2] []) [1 2]))
    (is (= (my-merge [] []) []))
    (is (= (my-merge [0 1 1 2] [1 2 2 3]) [0 1 1 1 2 2 2 3]))
    )
  )

(deftest msort-test
  (testing
      (is (=  [] (msort [])))
    (is (=  [1 2 3] (msort [1 2 3])))
    (is (=  [1 2 3] (msort [3 2 1])))
    (is (=  [1 2 3] (msort [2 3 1])))
    )
  )

(deftest my-replicate-rec-test
  (testing
      (is (=  [] (my-replicate-rec 0 "a")))
    (is (=  ["a"] (my-replicate-rec 1 "a")))
    (is (=  ["a" "a"] (my-replicate-rec 2 "a")))
    )
  )

(deftest my-test
  (testing (is (=  8 (my 2 3)))
    (is (=  1 (my 1 3)))
    (is (=  1 (my 0 0)))))

(deftest map-test
  (testing
      (is (=  [2 3 4] (my-map inc [1 2 3])))
    (is (=  [1 4 9] (my-map #(* % %) [1 2 3])))
    )
  )

(deftest filter-test
  (testing
      (is (=  [1 3] (my-filter odd? [1 2 3])))
    (is (=  [] (my-filter odd? [2])))
    (is (=  [] (my-filter odd? [])))
    )
  )

(deftest map-recur-test
  (testing
      (is (=  [2 3 4] (my-map-recur inc [1 2 3])))
    (is (=  [1 4 9] (my-map-recur #(* % %) [1 2 3])))
    )
  )

(deftest all-test
  (testing
      (is (=  true (my-all odd? [1 3 5])))
    (is (=  false (my-all odd? [1 2 5])))
    (is (=  false (my-all odd? [2 3 5])))
    (is (=  false (my-all odd? [1 3 4])))
    (is (=  true (my-all odd? [])))))

(deftest any-test
  (testing
      (is (=  true (my-any odd? [1 3 5])))
    (is (=  false (my-any odd? [2 4 6])))
    (is (=  false (my-any odd? [])))))

(deftest my-filter-recur-test
  (testing
      (is (=  [1 3] (my-filter-recur odd? [1 2 3])))
    (is (=  [] (my-filter-recur odd? [])))
    (is (=  [] (my-filter-recur odd? [2 4])))))

(deftest my-take-while-test
  (testing
      (is (=  [1] (my-take-while odd? [1 2 3])))
    (is (=  [1 3] (my-take-while odd? [1 3 4])))
    (is (=  [1 3 5] (my-take-while odd? [1 3 5])))
    (is (=  [] (my-take-while odd? [])))
    (is (=  [] (my-take-while odd? [2 3])))
    )
  )

(deftest my-foldr-test
  (testing
      (is (= 10 (my-foldr + 0 [1 2 3 4])))
    (is (= "abcdQ" (apply str (my-foldr concat "Q" ["a" "b" "c" "d"]))))))

(deftest my-foldl-test
  (testing
      (is (= 10 (my-foldl + 0 [1 2 3 4])))
    (is (= "Qabcd" (apply str (my-foldl concat "Q" ["a" "b" "c" "d"]))))))

(deftest bit2int-test
  (testing
      (is (==  0 (bits->int [0])))
    (is (==  1 (bits->int [1])))
    (is (==  2 (bits->int [0 1])))
    (is (==  3 (bits->int [1 1])))
    (is (==  4 (bits->int [0 0 1])))
    (is (==  5 (bits->int [1 0 1])))
    (is (==  6 (bits->int [0 1 1])))
    (is (==  7 (bits->int [1 1 1])))
    (is (==  8 (bits->int [0 0 0 1])))
    (is (==  9 (bits->int [1 0 0 1])))
    )
  )

(deftest int->bits-test
  (testing
      (is (= (int->bits 1) [1]))
    (is (= (int->bits 2) [0 1]))
    (is (= (int->bits 3) [1 1]))
    (is (= (int->bits 4) [0 0 1]))
    (is (= (int->bits 5) [1 0 1]))
    (is (= (int->bits 6) [0 1 1]))
    (is (= (int->bits 7) [1 1 1]))
    (is (= (int->bits 8) [0 0 0 1]))
    (is (= (int->bits 9) [1 0 0 1]))
    (is (= (int->bits 10) [0 1 0 1]))
    )
  )

(deftest make8-test
  (testing
      (is (=  [1 0 0 0 0 0 0 0] (make8 [1])))
      (is (=  [1 0 0 0 0 0 0 0] (make8 [1 0 0 0 0 0 0 0 0])))
      (is (=  [1 0 1 0 0 0 0 0] (make8 [1 0 1])))))

(deftest chop8-test
  (testing
      (is (= [[1 0 0 0 0 0 0 0] [0 0 0 0 0 0 0 0]] (chop8 [1 0 0 0 0 0 0 0 0])))
    (is (= [[1 0 0 0 0 0 0 0] [1 0 0 0 0 0 0 0]] (chop8 [1 0 0 0 0 0 0 0 1])))))

(deftest decode-test
  (testing
      (is (= "ab" (decode [1 0 0 0 0 1 1 0 0 1 0 0 0 1 1 0])))
    (is (= "ac" (decode [1 0 0 0 0 1 1 0 1 1 0 0 0 1 1 0])))
    )
  )

(deftest encode-test
  (testing
      (is (= [1 0 0 0 0 1 1 0 0 1 0 0 0 1 1 0] (encode "ab")))
    (is (= [1 0 0 0 0 1 1 0 1 1 0 0 0 1 1 0] (encode "ac")))
    )
  )

(deftest my-all-test
  (testing
      (is (= (my-all #(= 1 %) [5,4,3,2,1]) false))
    (is (= (my-all #(= 1 %) [1,1,1]) true))
    (is (= (my-all #(= 1 %) []) true))
    )
  )

(deftest my-any-test
  (testing
      (is (= (my-any #(= 1 %) [5,4,3,2,1]) true))
    (is (= (my-any #(= 1 %) [1,1,1]) true))
    (is (= (my-any #(= 1 %) []) false))
    )
  )

(deftest my-drop-while-test
  (testing
      (is (=  [2 3] (my-drop-while odd? [1 2 3])))
    (is (=  [4] (my-drop-while odd? [1 3 4])))
    (is (=  [] (my-drop-while odd? [1 3])))
    )
  )

(deftest crack-test
  (testing
      (is (= (crack (shift-string "i have attached work book, where we need to show two sheets in a dashboard." 3)) "i have attached work book, where we need to show two sheets in a dashboard."))
    (is (= (crack (shift-string "i have attached work book, where we need to show two sheets in a dashboard." 13)) "i have attached work book, where we need to show two sheets in a dashboard."))
    (is (= (crack (shift-string "i have attached work book, where we need to show two sheets in a dashboard." 23)) "i have attached work book, where we need to show two sheets in a dashboard."))
    )
  )

(deftest recently-modified?-test
  (testing
      (is (= true
             (let [f (java.io.File. "./resources/ut-recently-modified.txt")]
               (.setLastModified f (- (System/currentTimeMillis) (* 1000 60 29)))
               (recently-modified? f))))
    (is (= false
           (let [f (java.io.File. "./resources/ut-recently-modified.txt")]
             (.setLastModified f (- (System/currentTimeMillis) (* 1000 60 31)))
             (recently-modified? f))))))

(deftest my-set-test
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

(deftest deeply-nested-test
  (testing
      (is (= (deeply-nested 0) 'bottom))
    (is (= (deeply-nested 1) '(bottom)))
    (is (= (deeply-nested 2) '((bottom))))
    )
  )

(deftest count-heads-pairs-test
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

(deftest by-pairs-test
  (testing
      (is (= (by-pairs []) '()))
    (is (=  '() (by-pairs [:t])))
    (is (=  '((:t :t)) (by-pairs [:t :t])))
    (is (=  '((:t :t) (:t :t)) (by-pairs [:t :t :t])))
    (is (=  '((:t :t) (:t :t) (:t :t)) (by-pairs [:t :t :t :t])))
    (is (=  '((:h :t) (:t :t) (:t :h) (:h :h) (:h :h)) (by-pairs [:h :t :t :h :h :h])))
    )
  )

(deftest f-test
  (testing
      (is (= 1 (f 0)))
    (is (= 1 (f 1)))
    (is (= 2 (f 2)))
    (is (= 2 (f 3)))))

(deftest m-test
  (testing
      (is (= 0 (m 0)))
    (is (= 0 (m 1)))
    (is (= 1 (m 2)))
    (is (= 2 (m 3)))))

(deftest f-seq-test
  (testing
      (is (> 100 (elapsed-time (nth f-seq 250))))))

(deftest m-seq-test
  (testing
      (is (> 100 (elapsed-time (nth m-seq 250))))))

(deftest replace-symbol-test
  (is (=  [] (replace-symbol [] 'a 'b')))
  (is (=  ['b] (replace-symbol ['a] 'a 'b)))
  (is (=  ['b 'b] (replace-symbol ['a 'b] 'a 'b)))
  (is (=  ['b 'b] (replace-symbol ['b 'a] 'a 'b)))
  (is (=  [['b] 'b] (replace-symbol [['a] 'b] 'a 'b)))
  (is (=  ['b ['b]] (replace-symbol ['b ['a]] 'a 'b)))
  (is (=  ['b ['b] 'b] (replace-symbol ['a ['a] 'b] 'a 'b)))
  (is (=  [['b] [['b]] 'b] (replace-symbol [['a] [['a]] 'b] 'a 'b)))
  (is (= '((a a) (((a g r) (f r)) c (d e)) a)
         (replace-symbol '((a b) (((b g r) (f r)) c (d e)) b) 'b 'a)))
  (is (= (repeat 5 'b) (take 5 (replace-symbol (repeat 'a) 'a 'b)))))

(deftest make-greeter-test
  (testing
      (is (= "Hello, Yuji" ((make-greeter "Hello") "Yuji")))
    (is (= "Aloha, Yuji" ((make-greeter "Aloha") "Yuji")))
    )
  )

(deftest ut-recur-fibo
  (testing
      (is (=  0 (recur-fibo 0)))
    (is (=  1 (recur-fibo 1)))
    (is (=  1 (recur-fibo 2)))
    (is (=  2 (recur-fibo 3)))
    (is (=  3 (recur-fibo 4)))
    (is (=  5 (recur-fibo 5)))
    (is (=  8 (recur-fibo 6)))
    #_(is (< 1 (recur-fibo 1000000)))))

(deftest ut-lazy-seq-fibo
  (is (=  [0 1 1 2 3 5 8 13 21 34] (take 10 (lazy-seq-fibo))))
  #_(is (= (rem (nth (lazy-seq-fibo) 1000000) 1000) 875N)))

(deftest ut-clojure-loc
  (is (= 4 (clojure-loc (clojure.java.io/file (clojure.java.io/resource "clojure-loc")))))
  (is (= 2 (clojure-loc (clojure.java.io/file (clojure.java.io/resource "clojure-loc/baz.clj")))))
  (is (= 0 (clojure-loc (clojure.java.io/file (clojure.java.io/resource "clojure-loc/foo.txt"))))))

(deftest index-filter-test
  (is (=  #{0 1 4 5 6} (set (index-filter #{\a \b} "abcdbbb"))))
  (is (=  [] (index-filter #{\a \b} "xyz")))
  )

(deftest get-composer-test
  (is (=  ["J. S. Bach" "J. S. Bach" "W. A. Mozart"] (get-composer "./resources/compositions.xml"))))

(deftest my-and-test
  (is (my-and))
  (is (my-and true))
  (is (my-and true true))
  (is (not (my-and false)))
  (is (not (my-and true false)))
  (is (not (my-and false true))))

(deftest my-or-test
  (is (not (my-or)))
  (is (not (my-or false)))
  (is (not (my-or false false)))
  (is (my-or true))
  (is (my-or true false))
  (is (my-or false true))
  )

(deftest my-odd2?-test
  (is (= false (trampoline my-odd2? 0) ))
  (is (= true (trampoline my-odd2? 1)))
  (is (= false (trampoline my-odd2? 2)))
  (is (= true (trampoline my-odd2? 3)))
  (is (= false (trampoline my-odd2? 100000))))

(deftest my-even2?-test
  (is (= true (trampoline my-even2? 0)))
  (is (= false (trampoline my-even2? 1)))
  (is (= true (trampoline my-even2? 2)))
  (is (= false (trampoline my-even2? 3)))
  (is (= true (trampoline my-even2? 1000))))

(deftest list-files-test
  (is (= (list-files "./resources/ut-list-files/00") []))
  (is (= (set (list-files "./resources/ut-list-files/01")) #{"1" "a" "あ" "亜"}))
  (is (= (set (list-files "./resources/ut-list-files/02")) #{"dir01" "file01"}))
  (is (= (set (list-files "./resources/ut-list-files/03")) #{"dir_content_file01"}))
  )

(deftest chain-test
  (is (= "abc" (chain " abc " trim)))
  (is (= 3 (chain " abc " trim length)))
  )

(deftest tail-fibo-test
  (is (= 0 (tail-fibo 0)))
  (is (= 1 (tail-fibo 1)))
  (is (= 1 (tail-fibo 2)))
  (is (= 2 (tail-fibo 3)))
  (is (= 3 (tail-fibo 4)))
  (is (= 5 (tail-fibo 5)))
  (is (= 8 (tail-fibo 6)))
  (is (= 13 (tail-fibo 7)))
  )

(deftest my-with-out-str-test
  (is (= "hello, world!" (my-with-out-str (print "hello, ") (print "world!"))))
  )

(deftest count-not-empty-line-test
  (is (=  3 (count-not-empty-line (java.io.File. "./resources/ut-count-not-empty-line/01.txt"))))
  (is (=  0 (count-not-empty-line (java.io.File. "./resources/ut-count-not-empty-line/02.txt"))))
  (is (=  0 (count-not-empty-line (java.io.File. "./resources/ut-count-not-empty-line/03.txt"))))
  (is (=  0 (count-not-empty-line (java.io.File. "./resources/ut-count-not-empty-line/04.txt"))))
  (is (=  1 (count-not-empty-line (java.io.File. "./resources/ut-count-not-empty-line/05.txt"))))
  (is (=  1 (count-not-empty-line (java.io.File. "./resources/ut-count-not-empty-line/06.txt"))))
  (is (=  1 (count-not-empty-line (java.io.File. "./resources/ut-count-not-empty-line/07.txt"))))
  (is (=  1 (count-not-empty-line (java.io.File. "./resources/ut-count-not-empty-line/08.txt"))))
  )

(deftest count-runs-test
  (is (=  2 (count-runs 2 #(= :h %) [:h :t :t :h :h :h])))
  (is (=  1 (count-runs 2 #(= :t %) [:h :t :t :h :h :h])))
  (is (=  1 (count-runs 3 #(= :h %) [:h :t :t :h :h :h])))
  )

(deftest fibo-test
  (is (= '(0 1 1 2 3 5 8 13 21 34) (take 10 (fibo))))
  ;; パスするが、テスト実行時間が長くなるので普段はコメントアウトしている
  ;; (is (= (rem (nth (fibo) 1000000) 1000) 875N))
  )

(deftest stack-consuming-fibo-test
  (is (=  0 (stack-consuming-fibo 0)))
  (is (=  1 (stack-consuming-fibo 1)))
  (is (=  1 (stack-consuming-fibo 2)))
  (is (=  2 (stack-consuming-fibo 3)))
  (is (=  3 (stack-consuming-fibo 4)))
  (is (=  5 (stack-consuming-fibo 5)))
  (is (=  8 (stack-consuming-fibo 6)))
  (is (thrown? java.lang.StackOverflowError (stack-consuming-fibo 1000000N)))
  )

(deftest blank?-test
  (is (=  true (blank? " ")))
  (is (=  false (blank? "a")))
  (is (=  true (blank? "")))
  (is (=  false (blank? " a")))
  (is (=  false (blank? "a ")))
  (is (=  false (blank? " a ")))
  (is (=  true (blank? "　")))
  (is (=  true (blank? "  ")))
  )

(deftest bench-test
                                        ; (is (= (bench (str "a" "b")) {:result "ab", :elapsed 53026}))
  )

(deftest p22-test
  (is (=  5 (p22 '(1 2 3 3 1))))
  (is (=  11 (p22 "Hello World")))
  (is (=  3 (p22 [[1 2] [3 4] [5 6]])))
  (is (=  1 (p22 '(13))))
  (is (=  3 (p22 '(:a :b :c))))
  )

(deftest p23-test
  (is (=  [5 4 3 2 1] (p23 [1 2 3 4 5])))
  (is (=  '(7 5 2) (p23 (sorted-set 5 7 2 7))))
  (is (=  [[5 6][3 4][1 2]] (p23 [[1 2][3 4][5 6]])))
  )

(deftest p26-test
  (is (=  '(1 1 2) (p26 3)))
  (is (=  '(1 1 2 3 5 8) (p26 6)))
  (is (=  '(1 1 2 3 5 8 13 21) (p26 8)))
  )

(deftest p27-test
  (is (false? (p27 '(1 2 3 4 5))))
  (is (true? (p27 "racecar")))
  (is (true? (p27 [:foo :bar :foo])))
  (is (true? (p27 '(1 1 3 3 1 1))))
  (is (false? (p27 '(:a :b :c))))
  )

(deftest p28-test
  (is (=  '(1 2 3 4 5 6) (p28 '((1 2) 3 [4 [5 6]]))))
  (is (=  '("a" "b" "c") (p28 ["a" ["b"] "c"])))
  (is (=  '(:a) (p28 '((((:a)))))))
  )

(deftest p30-test
  (is (=  "Leroy" (apply str (p30 "Leeeeeerrroyyy"))))
  (is (=  '(1 2 3 2 3) (p30 [1 1 2 3 3 2 2 3])))
  (is (=  '([1 2] [3 4] [1 2]) (p30 [[1 2] [1 2] [3 4] [1 2]])))
  )

(deftest p31-test
  (is (=  '((1 1) (2) (1 1 1) (3 3)) (p31 [1 1 2 1 1 1 3 3])))
  (is (=  '((:a :a) (:b :b) (:c)) (p31 [:a :a :b :b :c])))
  (is (=  '(([1 2] [1 2]) ([3 4])) (p31 [[1 2] [1 2] [3 4]])))
  )

(deftest p32-test
  (is (=  '(1 1 2 2 3 3) (p32 [1 2 3])))
  (is (=  '(:a :a :a :a :b :b :b :b) (p32 [:a :a :b :b])))
  (is (=  '([1 2] [1 2] [3 4] [3 4]) (p32 [[1 2] [3 4]])))
  (is (=  '([1 2] [1 2] [3 4] [3 4]) (p32 [[1 2] [3 4]])))
  )

(deftest p33-test
  (is  (=  '(1 1 2 2 3 3) (p33  [1 2 3] 2)))
  (is  (=  '(:a :a :a :a :b :b :b :b) (p33  [:a :b] 4)))
  (is  (=  '(4 5 6) (p33  [4 5 6] 1)))
  (is  (=  '([1 2] [1 2] [3 4] [3 4]) (p33  [[1 2] [3 4]] 2)))
  (is  (=  [44 44 33 33] (p33  [44 33] 2)))
  )

(deftest p34-test
  (is  (=  '(1 2 3) (p34 1 4)))
  (is  (=  '(-2 -1 0 1) (p34 -2 2)))
  (is  (=  '(5 6 7) (p34 5 8)))
  )

(deftest p38-test
  (is (=  8 (p38 1 8 3 4)))
  (is (=  30 (p38 30 20)))
  (is (=  67 (p38 45 67 11)))
  )

(deftest p39-test
  (is (=  '(1 :a 2 :b 3 :c) (p39 [1 2 3] [:a :b :c])))
  (is (=  '(1 3 2 4) (p39 [1 2] [3 4 5 6])))
  (is (=  [1 5] (p39 [1 2 3 4] [5])))
  (is (=  [30 25 20 15] (p39 [30 20] [25 15])))
  )

(deftest p40-test
  (is (=  [1 0 2 0 3] (p40 0 [1 2 3])))
  (is (=  "one, two, three" (apply str (p40 ", " ["one" "two" "three"]))))
  (is (=  [:a :z :b :z :c :z :d] (p40 :z [:a :b :c :d])))
  )

(deftest p41-test
  (is (=  [1 2 4 5 7 8] (p41 [1 2 3 4 5 6 7 8] 3)))
  (is (=  [:a :c :e] (p41 [:a :b :c :d :e :f] 2)))
  (is (=  [1 2 3 5 6] (p41 [1 2 3 4 5 6] 4)))
  )

(deftest p42-test
  (is (=  1 (p42 1)))
  (is (=  6 (p42 3)))
  (is (=  120 (p42 5)))
  (is (=  40320 (p42 8)))
  )

(deftest p43-test
  (is (=  '((1 3 5) (2 4 6)) (p43 [1 2 3 4 5 6] 2)))
  (is (=  '((0 3 6) (1 4 7) (2 5 8)) (p43 (range 9) 3)))
  (is (=  '((0 5) (1 6) (2 7) (3 8) (4 9)) (p43 (range 10) 5)))
  )

(deftest p44-test
  (is (=  '(3 4 5 1 2) (p44 2 [1 2 3 4 5])))
  (is (=  '(4 5 1 2 3) (p44 -2 [1 2 3 4 5])))
  (is (=  '(2 3 4 5 1) (p44 6 [1 2 3 4 5])))
  (is (=  '(:b :c :a) (p44 1 '(:a :b :c))))
  (is (=  '(:c :a :b) (p44 -4 '(:a :b :c))))
  )

(deftest p50-test
  (is (=  #{[1 2 3] [:a :b :c]} (set (p50 [1 :a 2 :b 3 :c]))))
  (is (=  #{[:a :b] ["foo" "bar"]} (set (p50 [:a "foo"  "bar" :b]))))
  (is (=  #{[[1 2] [3 4]] [:a :b] [5 6]} (set (p50 [[1 2] :a [3 4] 5 6 :b]))))
  )

(deftest p53-test
  (is (=  [0 1 2 3] (p53 [1 0 1 2 3 0 4 5])))
  (is (=  [5 6] (p53 [5 6 1 3 2 7])))
  (is (=  [3 4 5] (p53 [2 3 3 4 5])))
  (is (=  [] (p53 [7 6 5 4])))
  )

(deftest p54-test
  (is (=  '((0 1 2) (3 4 5) (6 7 8)) (p54 3 (range 9))))
  (is (=  '((0 1) (2 3) (4 5) (6 7)) (p54 2 (range 8))))
  (is (=  '((0 1 2) (3 4 5)) (p54 3 (range 8))))
  )

(deftest p55-test
  (is (=  {1 4, 2 2, 3 1} (p55 [1 1 2 3 2 1 1])))
  (is (=  {:a 2, :b 3} (p55 [:b :a :b :a :b])))
  (is (=  {[1 2] 1, [1 3] 2} (p55 '([1 2] [1 3] [1 3]))))
  )

(deftest p56-test
  (is (=  [1 2 3 4] (p56 [1 2 1 3 1 2 4])))
  (is (=  [:a :b :c] (p56 [:a :a :b :b :c :c])))
  (is (=  '([2 4] [1 2] [1 3]) (p56 '([2 4] [1 2] [1 3] [1 3]))))
  (is (=  (range 50) (p56 (range 50))))
  )

(deftest p58-test
  (is (= [3 2 1] ((p58 rest reverse) [1 2 3 4])))
  (is (= 5 ((p58 (partial + 3) second) [1 2 3 4])))
  (is (= true ((p58 zero? #(mod % 8) +) 3 5 7 9)))
  (is (= "HELLO" ((p58 #(.toUpperCase %) #(apply str %) take) 5 "hello world")))
  )

(deftest p59-test
  (is (= [21 6 1] ((p59 + max min) 2 3 5 1 6 4)))
  (is (= ["HELLO" 5] ((p59 #(.toUpperCase %) count) "hello")))
  (is (= [2 6 4] ((p59 :a :c :b) {:a 2, :b 4, :c 6, :d 8 :e 10})))
  )

(deftest p60-test
  (is (=  [0 1 3 6 10] (take 5 (p60 + (range)))))
  (is (=  [[1] [1 2] [1 2 3] [1 2 3 4]] (p60 conj [1] [2 3 4])))
  (is (= 120 (reduce * 2 [3 4 5]) (last (p60 * 2 [3 4 5]))))
  )

(deftest p67-test
  (is (=  [2 3] (p67 2)))
  (is (=  [2 3 5 7 11] (p67 5)))
  (is (=  541 (last (p67 100))))
  )

(deftest p69-test
  (is (= {:a 4, :b 6, :c 20}
         (p69 * {:a 2, :b 3, :c 4} {:a 2} {:b 2} {:c 5})))
  (is (= {1 7, 2 10, 3 15}
         (p69 - {1 10, 2 20} {1 3, 2 10, 3 15})))
  (is (= {:a [3 4 5], :b [6 7], :c [8 9]}
         (p69 concat {:a [3], :b [6]} {:a [4 5], :c [8 9]} {:b [7]})))
  (is (= {:a [3 4 5], :b [6 7], :c [8 9 10]}
         (p69 concat {:a [3], :b [6]} {:a [4 5], :c [8 9]} {:b [7]} {:c [10]}))))

(deftest p74-test
  (is (=  "4,9" (p74 "4,5,6,7,8,9")))
  (is (=  "16,25,36" (p74 "15,16,25,36,37")))
  )

(deftest p75-test
  (is (=  1 (p75 1)))
  (is (=  (count '(1 3 7 9)) 4 (p75 10)))
  (is (=  16 (p75 40)))
  (is (=  60 (p75 99)))
  )

(deftest p77-test
  (is (= #{#{"meat" "team" "mate"}}
         (p77 ["meat" "mat" "team" "mate" "eat"])))
  (is (= #{#{"veer" "ever"} #{"lake" "kale"} #{"mite" "item"}}
         (p77 ["veer" "lake" "item" "kale" "mite" "ever"])))
  )

(deftest p78-test
  (is (= 82
         (letfn [(triple [x] #(sub-two (* 3 x)))
                 (sub-two [x] #(stop?(- x 2)))
                 (stop? [x] (if (> x 50) x #(triple x)))]
           (p78 triple 2))))
  (is (= [true false true false true false]
         (letfn [(my-even? [x] (if (zero? x) true #(my-odd? (dec x))))
                 (my-odd? [x] (if (zero? x) false #(my-even? (dec x))))]
           (map (partial p78 my-even?) (range 6)))))
  )

(deftest p80-test
  (is (=  true (p80 6)))
  (is (=  false (p80 7)))
  (is (=  true (p80 496)))
  (is (=  false (p80 500)))
  (is (=  true (p80 8128)))
  )

(deftest p86-test
  (is (=  true (p86 7)))
  (is (=  true (p86 986543210)))
  (is (=  false (p86 2)))
  (is (=  false (p86 3)))
  )

(deftest p95-test
  (is (= 
       true
       (p95 [:a [:b nil nil] nil])))
  (is (= 
       false
       (p95 [:a [:b nil nil]])))
  (is (= 
       true
       (p95 [1 nil [2 [3 nil nil] [4 nil nil]]])))
  (is (= 
       false
       (p95 [1 [2 nil nil] [3 nil nil] [4 nil nil]])))
  (is (= 
       true
       (p95 [1 [2 [3 [4 nil nil] nil] nil] nil])))
  (is (= 
       false
       (p95 [1 [2 [3 [4 false nil] nil] nil] nil])))
  (is (= 
       false
       (p95 '(:a nil ()))))
  )

(deftest p96-test
  (is (=  true
          (p96 [:a [:b nil nil] [:b nil nil]])))
  (is (=  false 
          (p96 [:a [:b nil nil] nil])))
  (is (=  false 
          (p96 [:a [:b nil nil] [:c nil nil]])))
  (is (= 
       true
       (p96 [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
             [2 [3 nil [4 [6 nil nil] [5 nil nil]]] nil]])))
  (is (= 
       false
       (p96 [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
             [2 [3 nil [4 [5 nil nil] [6 nil nil]]] nil]])))
  (is (= 
       false
       (p96 [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
             [2 [3 nil [4 [6 nil nil] nil]] nil]])))
  )

(deftest p97-test
  (is (=  [1] (p97 1)))
  (is (= 
       [     [1]
        [1 1]
        [1 2 1]
        [1 3 3 1]
        [1 4 6 4 1]]
       (map p97 (range 1 6))))
  (is (= 
       [1 10 45 120 210 252 210 120 45 10 1]
       (p97 11)))
  )

(deftest p98-test
  (is (= 
       #{#{0} #{1 -1} #{2 -2}}
       (p98 #(* % %) #{-2 -1 0 1 2})))
  (is (= 
       #{#{0 3} #{1 4} #{2 5}}
       (p98 #(rem % 3) #{0 1 2 3 4 5 })))
  (is (= 
       #{#{0} #{1} #{2} #{3} #{4}}
       (p98 identity #{0 1 2 3 4})))
  (is (= 
       #{#{0 1 2 3 4}}
       (p98 (constantly true) #{0 1 2 3 4})))
  )

(deftest p100-test
  (is (== (p100 2 3) 6))
  (is (== (p100 5 3 7) 105))
  (is (== (p100 1/3 2/5) 2))
  (is (== (p100 3/4 1/6) 3/2))
  (is (== (p100 7 5/7 2 3/5) 210))
  )

(deftest p102-test
  (is (=  "something" (p102 "something")))
  (is (=  "multiWordKey" (p102 "multi-word-key")))
  (is (=  "leaveMeAlone" (p102 "leaveMeAlone")))
  )

(deftest p115-test
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

(deftest p135-test
  (is (= (p135 2 + 5) 7  ))
  (is (= (p135 38 + 48 - 2 / 2) 42 ))
  (is (= (p135 10 / 2 - 1 * 2) 8  ))
  (is (= (p135 20 / 2 + 2 + 4 + 8 - 6 - 10 * 9) 72 ))
  )

(deftest p146-test
  (is (= 
         '{[a p] 1, [a q] 2
           [b m] 3, [b n] 4}
         (p146 '{a {p 1, q 2}
                 b {m 3, n 4}})))
  (is (= 
         '{[[1] a] b, [[1] c] d,
           [[2] q] r, [[2] s] t,
           [[2] u] v, [[2] w] x}
         (p146 '{[1] {a b c d}
                 [2] {q r s t u v w x}})))
  (is (= 
       '{[m 1] [a b c], [m 3] nil}
       (p146 '{m {1 [a b c] 3 nil}})))
  )

(deftest p147-test
  (is (=  [2 5 5 2] (second (p147 [2 3 2]))))
  (is (=  [[1] [1 1] [1 2 1] [1 3 3 1] [1 4 6 4 1]] (take 5 (p147 [1]))))
  (is (=  [[3 1 2] [3 4 3 2]] (take 2 (p147 [3 1 2]))))
  (is (=  (rest (take 101 (p147 [2 2]))) (take 100 (p147 [2 4 2]))))
  )

(deftest p85-test
  (is (=  #{#{1 :a} #{:a} #{} #{1}}
          (p85 #{1 :a})))
  (is (=  #{#{}}
          (p85 #{})))
  (is (= 
       #{#{} #{1} #{2} #{3} #{1 2} #{1 3} #{2 3} #{1 2 3}}
       (p85 #{1 2 3})))
  (is (=  1024
          (count (p85 (into #{} (range 10))))))
  )

(deftest p105-test
  (is (= {} (p105 [])))
  (is (= {:a [1]} (p105 [:a 1])))
  (is (= {:a [1], :b [2]} (p105 [:a 1, :b 2])))
  (is (= {:a [1 2 3], :b [], :c [4]} (p105 [:a 1 2 3 :b :c 4])))
  )

(deftest p137-test
  (is (= [1 2 3 4 5 0 1] (p137 1234501 10)))
  (is (= [0] (p137 0 11)))
  (is (= [1 0 0 1] (p137 9 2)))
  (is (= [1 0] (let [n (rand-int 100000)](p137 n n))))
  (is (= [16 18 5 24 15 1] (p137 Integer/MAX_VALUE 42)))
  )

(deftest p110-test
  (is (= [[1 1] [2 1] [1 2 1 1]] (take 3 (p110 [1]))))
  (is (= [3 1 2 4] (first (p110 [1 1 1 4 4]))))
  (is (= [1 1 1 3 2 1 3 2 1 1] (nth (p110 [1]) 6)))
  (is (= 338 (count (nth (p110 [3 2]) 15))))
  )

(deftest p144-test
  (is (= (take 3 (p144 3.14 int double)) [3.14 3 3.0]))
  (is (= (take 5 (p144 3 #(- % 3) #(+ 5 %))) [3 0 5 2 7]))
  (is (= (take 12 (p144 0 inc dec inc dec inc)) [0 1 0 1 0 1 2 1 2 1 2 3]))
  )

(deftest p108-test
  (is (= 3 (p108 [3 4 5])))
  (is (= 4 (p108 [1 2 3 4 5 6 7] [0.5 3/2 4 19])))
  (is (= 7 (p108 (range) (range 0 100 7/6) [2 3 5 7 11 13])))
  (is (= 64 (p108 (map #(* % % %) (range)) ;; perfect cubes
                  (filter #(zero? (bit-and % (dec %))) (range)) ;; powers of 2
                  (iterate inc 20)))) ;; at least as large as 20
  )

(deftest p93-test
  (is (= 
       [["Do"] ["Nothing"]]
       (p93 [["Do"] ["Nothing"]])))
  (is (= 
       [[:a :b] [:c :d] [:e :f]]
       (p93 [[[[:a :b]]] [[:c :d]] [:e :f]])))
  (is (= 
       '((1 2)(3 4)(5 6))
       (p93 '((1 2)((3 4)((((5 6)))))))))
  )

(deftest p158-test
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

(deftest p114-test
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

(deftest p132-test
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

(deftest p104-test
  (is (= "I" (p104 1)))
  (is (= "XXX" (p104 30)))
  (is (= "IV" (p104 4)))
  (is (= "CXL" (p104 140)))
  (is (= "DCCCXXVII" (p104 827)))
  (is (= "MMMCMXCIX" (p104 3999)))
  (is (= "XLVIII" (p104 48)))
  )

(deftest p92-test
  (is (= 14 (p92 "XIV")))
  (is (= 827 (p92 "DCCCXXVII")))
  (is (= 3999 (p92 "MMMCMXCIX")))
  (is (= 48 (p92 "XLVIII")))
  )

(deftest p103-test
  (is (= (p103 1 #{4 5 6}) #{#{4} #{5} #{6}}))
  (is (= (p103 10 #{4 5 6}) #{}))
  (is (= (p103 2 #{0 1 2}) #{#{0 1} #{0 2} #{1 2}}))
  (is (= (p103 3 #{0 1 2 3 4}) #{#{0 1 2} #{0 1 3} #{0 1 4} #{0 2 3} #{0 2 4}
                                 #{0 3 4} #{1 2 3} #{1 2 4} #{1 3 4} #{2 3 4}}))
  (is (= (p103 4 #{[1 2 3] :a "abc" "efg"}) #{#{[1 2 3] :a "abc" "efg"}}))
  (is (= (p103 2 #{[1 2 3] :a "abc" "efg"}) #{#{[1 2 3] :a} #{[1 2 3] "abc"} #{[1 2 3] "efg"}
                                              #{:a "abc"} #{:a "efg"} #{"abc" "efg"}}))
  )

(deftest p116-test
  (is (= false (p116 4)))
  (is (= true (p116 5)))
  (is (= true (p116 53)))
  (is (= true (p116 563)))
  (is (= true (p116 1103)))
  (is (= 1103 (nth (filter p116 (range)) 15)))
  )

#_(deftest configured-city-test
    (is (= (configured-city) "Tokyo")))
