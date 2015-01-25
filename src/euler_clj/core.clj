(ns euler-clj.core
  (:require [clojure.math.combinatorics :as combo]
            [clojure.math.numeric-tower :as math]
            [clj-time.core :as t]
            [clj-time.predicates :as pr]
            [com.hypirion.primes :as prime]))

(load "problem-42-data")

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


(defn find-factors [n]
  "find factors of n
   ex: 28 -> (1 2 4 7 14)"
  (filter #(== (rem n %) 0) (range 1 (inc (quot n 2)))))


(defn sum-of-factors [n]
  "find some of factors of n
   ex: 28 -> (1 2 4 7 14) -> 28"
  (apply + (find-factors n)))


(defn abundant? [n]
  (> (sum-of-factors n) n))


(defn prime-factors [n]
  ; find prime factors of n
  ; THIS IS NOT INTEGER FACTORIZATION ALGORITHM
  ; ex: (prime-factors 12341) -> (7 41 43)
  (let [divisors (take-while #(>= (quot n 2) %) (prime/primes))]
    (filter #(== 0 (rem n %)) divisors)))


(defn palindrome? [s]
  (= s (apply str (reverse s))))


(defn pandigital? [n]
  (== (count (decompose-digits n))
      (count (distinct (decompose-digits n)))))


(defn right-triangle? [a b c]
  ; c: hypotenuse
  (== (+ (* a a) (* b b)) (* c c)))


(defn find-rt [n] ; this function for  problem 39
  (for [a (range 1 (quot n 2)), b (range 1 (quot n 2))
    :when (and (<= a b) (right-triangle? a b (- n a b)))]
    (list a b)))


; test n is pentagonal number
(defn pentagonal? [n]
  (integer? (/ (inc (math/sqrt (inc (* 24 n)))) 6)))

(def pentagonals
  (map #(quot (* % (dec (* 3 %))) 2)  (range)))


(def triangulars
  (map #(quot (* % (inc %)) 2)  (range)))


(def hexagonals
  (map #(* % (dec (* 2 %)))  (range)))


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


(defn problem-15 []
  (let [n 20]
    (/ (fact (*' 2 n)) (*' (fact n) (fact n)))))


(defn problem-17 []
  (let [digits (combo/cartesian-product (range 10) (range 10) (range 10))
        first-digit (fn [a1]
          (cond
            (== 0 a1) ""
            (== 1 a1) "onehundred"
            (== 2 a1) "twohundred"
            (== 3 a1) "threehundred"
            (== 4 a1) "fourhundred"
            (== 5 a1) "fivehundred"
            (== 6 a1) "sixhundred"
            (== 7 a1) "sevenhundred"
            (== 8 a1) "eighthundred"
            (== 9 a1) "ninehundred"))

        third-digit (fn [a3]
          (cond
            (== 0 a3) ""
            (== 1 a3) "one"
            (== 2 a3) "two"
            (== 3 a3) "three"
            (== 4 a3) "four"
            (== 5 a3) "five"
            (== 6 a3) "six"
            (== 7 a3) "seven"
            (== 8 a3) "eight"
            (== 9 a3) "nine"))

        second-digit (fn [a2 a3]
          (if (== a2 1)
            (cond
              (== 0 a3) "ten"
              (== 1 a3) "eleven"
              (== 2 a3) "twelve"
              (== 3 a3) "thirteen"
              (== 4 a3) "fourteen"
              (== 5 a3) "fifteen"
              (== 6 a3) "sixteen"
              (== 7 a3) "seventeen"
              (== 8 a3) "eighteen"
              (== 9 a3) "nineteen")
            (cond
              (== 0 a2) (third-digit a3)
              (== 2 a2) (str "twenty" (third-digit a3))
              (== 3 a2) (str "thirty" (third-digit a3))
              (== 4 a2) (str "forty" (third-digit a3))
              (== 5 a2) (str "fifty"  (third-digit a3))
              (== 6 a2) (str "sixty"  (third-digit a3))
              (== 7 a2) (str "seventy" (third-digit a3))
              (== 8 a2) (str "eighty" (third-digit a3))
              (== 9 a2) (str "ninety" (third-digit a3)) )))


        add-and (fn [a1 a2 a3]
          (if (and (not= a1 0) (or (not= a2 0) (not= a3 0)))
              "and"
              ""))

        compose-letter (fn [digits]
          (str (first-digit (nth digits 0))
               (add-and (nth digits 0) (nth digits 1) (nth digits 2))
               (second-digit (nth digits 1) (nth digits 2))))]

    ; main parts
    (+ 11 (count (apply str (map compose-letter digits))))))



(defn problem-19 []
  (let [a (map
            #(t/date-time (+ 1901 (nth % 0)) (nth % 1) 1)
            (combo/cartesian-product (range 100) (range 1 13) ))] ; 1901/1/1, 1901/2/1,...,2000/12/1
    (count (filter pr/sunday? a))))


(defn problem-23 []
  (let [N 28123
        abundants (filter abundant? (range 1 (+ N 1)))]
    (->> (for [x abundants] (map #(+ x %) abundants))
         (map (fn [ns]
                (take-while (fn [n] (<= n N)) ns)))
         (map set)
         (reduce clojure.set/union)
         (reduce +)
         (- (/ (* N (+ N 1)) 2))))) ; answer: 4179871


(defn problem-29 []
  (let [M (range 2 101)]
    (count (distinct (apply concat (for [n M] (map #(math/expt n %) M)))))))


(defn problem-30 []
   (let [L (range 2 354300)
         expt-5 #(math/expt % 5)]
      (apply + (filter #(== % (apply +  (map expt-5  (decompose-digits %)) )) L))))


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


(defn problem-36 []
  (letfn [
    (palindrome-1 []
      (range 1 10))

    (palindrome-2 []
      (map #(Integer/parseInt %) (for [n (range 1 10)]
        (str n n))))

    (palindrome-3 []
      (apply concat (for [m (range 10)]
        (for [n (range 1 10)]
          (compose-digits (concat (decompose-digits n) (list m) (reverse (decompose-digits n))))))))

    (palindrome-4 []
      (for [n (range 10 100)]
        (compose-digits (concat (decompose-digits n) (reverse (decompose-digits n))))))

    (palindrome-5 []
      (apply concat (for [m (range 10)]
        (for [n (range 10 100)]
          (compose-digits (concat (decompose-digits n) (list m) (reverse (decompose-digits n))))))))

    (palindrome-6 []
      (for [n (range 100 1000)]
        (compose-digits (concat (decompose-digits n) (reverse (decompose-digits n))))))

    (filter-36 [s]
      (map #(Integer/parseInt % 2) (filter palindrome? (map #(Integer/toString % 2) (s)))))]

      (apply + (concat (filter-36 palindrome-1)
              (filter-36 palindrome-2)
              (filter-36 palindrome-3)
              (filter-36 palindrome-4)
              (filter-36 palindrome-5)
              (filter-36 palindrome-6)))))


(defn problem-37 []
  (let [primes (generate-primes 1000000)
        parts-37 (fn [n]
          (for [i (range 1 (inc (count (str n))))]
             (compose-digits (take i (decompose-digits n))  )))

        parts-37-r (fn [n]
          (for [i (range 1 (inc (count (str n))))]
             (compose-digits (reverse (take i (reverse (decompose-digits n)))  ))))

        prime? (fn [n primes]
          (loop [primes primes]
            (cond
              (or (not (seq primes)) (> (first primes) n)) false
              (==  (first primes) n) true
              :else (recur (rest primes)))))

        filter-37 (fn [n primes]
          (and (every? #(prime? % primes) (parts-37 n))
               (every? #(prime? % primes) (parts-37-r n))))]

    (reduce + (drop 4 (filter #(filter-37 % primes) primes))))) ; 748317


(defn problem-38 []
  (let [gen-38 (fn [n]
    (loop [out (), i 1]
      (cond
        (and (== (count out) 9) (== (count (distinct out)) 9) (== (.indexOf out 0) -1)) (compose-digits out)
        (>= (count out) 9) 0
        :else (recur (concat out (decompose-digits (* i n))) (inc i) ))))]
    (apply max (map gen-38 (range 1 10000)))))


(defn problem-40 []
  (let [gen-40'' (fn [n]
          (loop [out "" i 1]
            (if (> i n)
              out
              (recur (str out (str i)) (inc i)))))

        s (gen-40'' 190000)]

    (* (Integer/parseInt (str (nth s 0)))
       (Integer/parseInt (str (nth s 9)))
       (Integer/parseInt (str (nth s 99)))
       (Integer/parseInt (str (nth s 999)))
       (Integer/parseInt (str (nth s 9999)))
       (Integer/parseInt (str (nth s 99999)))
       (Integer/parseInt (str (nth s 999999))))))


(defn problem-41 []
  (let [candidates (map compose-digits (combo/permutations (range 1 8)))]
    (last (filter #(and (pandigital? %) (prime/prime? %)) candidates))))


(defn problem-42 []
  (let [triangle (map #(quot (* % (inc %)) 2) (range))
        triangle? (fn [n] (== (last (take-while (partial >= n) triangle)) n))
        word-value (fn [s] (apply + (map #(- (int %) 64) (seq (char-array s)))))]
    (count (filter #(triangle? (word-value %)) problem-42-data))))


(defn problem-43 []
  (let [gen-43
          (fn [n]
            (->> (range)
                 (map #(* n %))
                 (drop-while #(< % 100))
                 (take-while #(< % 1000))
                 (filter pandigital?)
                 (map #(decompose-digits %))))

        cons-43
          (fn [modulo l]
            (for [e l
                  n (range 10)
              :let [target (compose-digits (cons n (take 2 e)))]
              :when (and (==  (rem target modulo)  0)
                         (pandigital? target))]
              (cons n e)))

        list-43
          (->> (gen-43 17)
               (cons-43 13)
               (cons-43 11)
               (cons-43 7)
               (cons-43 5)
               (cons-43 3)
               (cons-43 2))]
    (reduce + (for [l list-43,
          n (range 10)
          :let [ans (compose-digits (cons n l))]
          :when (and (> ans 999999999) (pandigital? ans))]
          ans)))) ; Elapsed time: 30.846896 msecs 16695334890


(defn problem-44 []
  (loop [target '()
         l pentagonals]
    (let [candidates (filter #(and (pentagonal? (- (first l) %)) (pentagonal? (+ (first l) %))) target) ]
      (if (> (count candidates) 0)
        (- (first l) (first candidates))
        (recur (cons (first l) target) (rest l))))))


(defn problem-45 []
  (last (filter pentagonal? (take 1000000 hexagonals))))


(defn problem-46 []
  (let [double-pow ((map #(* 2 (* % %)) (range)))
        primes (generate-primes 6000)
        l (take-while #(< % 6000) double-pow)
        candidates (->> primes
                        (take-while #(< % 6000))
                        (combo/cartesian-product l)
                        (map #(apply + %))
                        (distinct)
                        (sort)
                        (filter odd?))]

    (loop [cur candidates]
      (if (== -2 (- (first cur) (nth cur 1)))
        (recur (rest cur))
        (- (nth cur 1) 2)))))


(defn problem-47 [] ; This function is so slow... take about 5min
  (let [candidates (filter #(== 4 (count (prime-factors %))) (range))]
    (loop [l candidates]
      (if (== 3 (- (nth l 3) (first l)))
        (take 4 l)
        (recur (rest l))))))


(defn problem-48 []
  (- (reduce + (take 1001 (map #(math/expt % %) (range)))) 1))


(defn problem-49 []
  (let [pandigital-primes
          (take-while #(> 10000 %) (drop-while #(> 1000 %) (prime/primes)))

        cp (combo/cartesian-product pandigital-primes pandigital-primes)
        candidates
          (filter #(and 
                     (< (first %) (second %))
                     (not= (first %) (second %)) 
                     (== 4 (count (distinct (concat (decompose-digits (first %)) (decompose-digits (second %)))))))
                     cp)]
    (->> (filter #(and 
               (pandigital? (+ (second %) (- (second %) (first %))))
               (== 4 (count (distinct (concat (decompose-digits (second %)) (decompose-digits (+ (second %) (- (second %) (first %))))))))
               (prime/prime? (+ (second %) (- (second %) (first %))))) candidates)
        (map #(cons  (+ (second %) (- (second %) (first %))) %)))))
