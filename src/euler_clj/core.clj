(ns euler-clj.core
  (:require [clojure.math.combinatorics :as combo])
  (:require [clojure.math.numeric-tower :as math]))


; Utility Functions
(defn fact [n]
  "Calculate Factorial"
  (reduce *' (range 1 (inc n))))


(defn decompose-digits [n]
  "decompose integer to digits.
   ex: 321 -> (3 2 1)"
  (loop [n n, digits ()]
    (if (== n 0)
      digits
      (recur (quot n 10) (cons (rem n 10) digits)))))


(defn generate-primes [n]
  "generate prime numbers under n"
  (loop [candidates (range 3 (inc n) 2), ; even numbers is not prime
         primes ()]
    (if (> (first candidates) (math/sqrt n))
      (sort (cons 2 (concat primes candidates)))
      (recur (remove #(zero? (mod % (first candidates))) (rest candidates))
             (cons (first candidates) primes) ))))


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
