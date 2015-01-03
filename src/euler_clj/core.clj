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
    (apply + (filter filter-34 (range 10 100000))))) ; answer: 40730



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
             (filter (partial filter-35 primes) primes)))))
