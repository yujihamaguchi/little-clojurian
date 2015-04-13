; Q: 以下の様に使用できる関数foldrを自作せよ。
; cons = foldr (:) []
; sum = foldr (+) 0
; product = foldr (*) 1
; or = foldr (||) False
; and = foldr (&&) True
; A:
; myFoldr :: (a -> b -> b) -> b -> [a] -> b
; myFoldr _ x' [] = x'
; myFoldr f x' (x:xs) = f x (myFoldr f x' xs)
(defn myFoldr
	([f x] x)
	([f x xs] (f (first xs) (myFoldr f x (rest xs))))))