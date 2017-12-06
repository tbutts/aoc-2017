(ns aoc-2017.03.core
  (:require [aoc-2017.common :as aoc]
            [clojure.test :refer [are is with-test]]))

(defn squared [n] (* n n))
(def odd-squares (map squared (filter odd? (range))))
(defn next-odd-square [x] (first (filter (partial < x) odd-squares)))

(defn true-offset
  [off max-dist]
  (if (> off (/ max-dist 2))
    (- max-dist off)
    off))

(defn manhattan-dist
  [x]
  (let [max (next-odd-square x) ; Get the highest value in this ring (always (2x+1)^2)
        max-dist (->> (Math/sqrt max) long dec) ; max possible man distance from center
        off (-> (- max x) (mod max-dist) (true-offset max-dist)) ] ; Offset, adjusted by projecting based on orientation to the diagonal and axis
    (- max-dist off)))

(with-test
  (def pt1-examples)

  (are [in out] (= (manhattan-dist in) out)
    ;1 1
    12 3
    23 2
    1024 31))


(def day "03")
(def challenge-input 325489)

(def get-solution-01 #(manhattan-dist challenge-input))

(defn -main
  [& args]
  (aoc/run day [get-solution-01]))

; pt01 = 552
