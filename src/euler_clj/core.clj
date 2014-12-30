(ns euler-clj.core
  (:require [clojure.math.combinatorics :as combo]))

(def p (combo/permutations (vec (range 1 10 1))))

; シーケンス a がパンデジタル数であるか？
(defn is_pandigital [s]
  (let [m  (+                                      (* 10 (nth s 0)) (nth s 1))
        n  (+                    (* 100 (nth s 2)) (* 10 (nth s 3)) (nth s 4))
        mn (+ (* 1000 (nth s 5)) (* 100 (nth s 6)) (* 10 (nth s 7)) (nth s 8))]
    (println m n mn)
    (= (* m n) mn)
    ))


