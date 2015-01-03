(ns euler-clj.core
  (:require [clojure.math.combinatorics :as combo])
  (:require [clojure.math.numeric-tower :as math]))


; Utility Functions
(defn fact [n]
  "Calculate Factorial"
  (reduce *' (range 1 (inc n))))


(defn decompose-digits [n]
  "decompose integer to digits list.
   ex: 321 -> (3 2 1)"
  (loop [n n, digits ()]
    (if (== n 0)
      digits
      (recur (quot n 10) (cons (rem n 10) digits)))))


(defn compose-digits [s]
  "compose digits list to integer.
   ex: (1 2 3) -> 123"
  (loop [s (reverse s), coefficient 1, ans 0]
    (if (seq s)
      (recur (rest s) (* 10 coefficient) (+ ans (* coefficient (first s))))
      ans)))


(defn count-digits [n]
  "return digits size of an integer
   ex: 123 -> 3"
  (count (decompose-digits n)))


(defn generate-primes [n]
  "generate prime numbers under n"
  ; TODO: This function is so slow
  (loop [candidates (range 3 (inc n) 2), ; even numbers is not prime
         primes ()]
    (if (> (first candidates) (math/sqrt n))
      (sort (cons 2 (concat primes candidates)))
      (recur (remove #(zero? (mod % (first candidates))) (rest candidates))
             (cons (first candidates) primes) ))))


(defn lotate-digits [n]
  "lotate integer digits.
   ex: 1234 -> 2341"
  (let [a (reverse (rest (decompose-digits n))),
        b (first (decompose-digits n))]
    (compose-digits (reverse  (cons b a)))))


; Problem Solver Implementations
(defn problem-11 []
  (let [array-11
         [[ 8  2 22 97 38 15  0 40  0 75  4  5  7 78 52 12 50 77 91  8]
          [49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48  4 56 62  0]
          [81 49 31 73 55 79 14 29 93 71 40 67 53 88 30  3 49 13 36 65]
          [52 70 95 23  4 60 11 42 69 24 68 56  1 32 56 71 37  2 36 91]
          [22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80]
          [24 47 32 60 99  3 45  2 44 75 33 53 78 36 84 20 35 17 12 50]
          [32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70]
          [67 26 20 68  2 62 12 20 95 63 94 39 63  8 40 91 66 49 94 21]
          [24 55 58  5 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72]
          [21 36 23  9 75  0 76 44 20 45 35 14  0 61 33 97 34 31 33 95]
          [78 17 53 28 22 75 31 67 15 94  3 80  4 62 16 14  9 53 56 92]
          [16 39  5 42 96 35 31 47 55 58 88 24  0 17 54 24 36 29 85 57]
          [86 56  0 48 35 71 89  7  5 44 44 37 44 60 21 58 51 54 17 58]
          [19 80 81 68  5 94 47 69 28 73 92 13 86 52 17 77  4 89 55 40]
          [ 4 52  8 83 97 35 99 16  7 97 57 32 16 26 26 79 33 27 98 66]
          [88 36 68 87 57 62 20 72  3 46 33 67 46 55 12 32 63 93 53 69]
          [ 4 42 16 73 38 25 39 11 24 94 72 18  8 46 29 32 40 62 76 36]
          [20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74  4 36 16]
          [20 73 35 29 78 31 90  1 74 31 49 71 48 86 81 16 23 57  5 54]
          [ 1 70 54 71 83 51 54 69 16 92 33 48 61 43 52  1 89 19 67 48]]

        search-line (fn [m]
          (apply max
            (for [y (range 20)]
              (let [l (nth m y)]
                (apply max
                  (for [x (range 17)]
                    (* (nth l x) (nth l (+ 1 x)) (nth l (+ 2 x)) (nth l (+ x 3)))))))))

        search-diagonal (fn [m]
          (apply max
            (for [y (range 17)]
              (apply max
                (for [x (range 17)]
                  (* (nth (nth m y) x)
                     (nth (nth m (+ y 1)) (+ x 1))
                     (nth (nth m (+ y 2)) (+ x 2))
                     (nth (nth m (+ y 3)) (+ x 3))))))))

        search-diagonal-2 (fn [m]
          (apply max
            (for [y (range 17)]
              (apply max
                (for [x (range 3 20)]
                  (* (nth (nth m y) x)
                     (nth (nth m (+ y 1)) (- x 1))
                     (nth (nth m (+ y 2)) (- x 2))
                     (nth (nth m (+ y 3)) (- x 3))))))))]

    (max (search-line array-11)
         (search-diagonal array-11)
         (search-diagonal-2 array-11)))) ; answer: 70600674


(defn problem-32 []
  (let [p (combo/permutations (range 1 10))

        ; ex: 12 x 483 = 5796
        is-pandigital-2-3-4 (fn [s]
          (let [m  (+                                      (* 10 (nth s 0)) (nth s 1))
                n  (+                    (* 100 (nth s 2)) (* 10 (nth s 3)) (nth s 4))
                mn (+ (* 1000 (nth s 5)) (* 100 (nth s 6)) (* 10 (nth s 7)) (nth s 8))]
            (if (= (* m n) mn) mn false)))

        ; ex: 4 x 1963 = 7852
        is-pandigital-1-4-4 (fn [s]
          (let [k  (nth s 0)
                l  (+ (* 1000 (nth s 1)) (* 100 (nth s 2)) (* 10 (nth s 3)) (nth s 4))
                kl (+ (* 1000 (nth s 5)) (* 100 (nth s 6)) (* 10 (nth s 7)) (nth s 8))]
            (if (= (* k l) kl) kl false)))]

    (loop [p p ans ()]
      (if (seq p)
        (let [n (or (is-pandigital-2-3-4 (first p)) (is-pandigital-1-4-4 (first p)))]
          (if n
            (recur (rest p) (cons n ans))
            (recur (rest p) ans)))
        (apply + (distinct ans)))))) ; answer: 45228


(defn problem-33 []
  (letfn [(gen-denomi [a]
            (map #(list % (+ (* 10 %) a)) (range 1 10)))

          (gen-numera [a]
            (map #(list % (+ (* 10 a) %)) (range 1 10)))

          (filter-33 [k]
            (== (/ (first (first k))   (first (second k)))
                (/ (second (first k))  (second (second k)))))]

    (for [x (range 1 10)
          :let [denomi (gen-denomi x)
                numera (gen-numera x)]]
          (filter filter-33 (combo/cartesian-product denomi numera))))) ; answer: 100


(defn problem-34 []
  (letfn [(filter-34 [n] (==  (apply + (map fact (decompose-digits n))) n))]
    (apply + (filter filter-34 (range 10 100000))))) ; answer: 4 730



(defn problem-35 []
  (let [primes (generate-primes 1000000)

        filter-35 (fn [primes n]
          (every?
            #(not= (.indexOf primes %) -1)
            (take (count-digits n) (iterate lotate-digits n))))

        filter-contain-zero-digits (fn [n]
          (== (.indexOf (decompose-digits n) 0) -1))]

    (count (filter
             filter-contain-zero-digits
             (filter (partial filter-35 primes) primes))))) ; answer: 55
