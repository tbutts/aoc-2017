(ns aoc-2017.22.core
  (:require
   [clojure.test :refer [are is with-test]]
   [clojure.string :as str])
  (:use [aoc-2017.common]))

(def dirs-map
  {:n [-1 0]
   :e [0 1]
   :s [1 0]
   :w [0 -1]})

(def turns-map
  "Maps cardinal dirs to their rotated positions"
  (into {} (apply map #(vector %1 {:left %2 :right %3 :forward %1 :reverse %4})
                  ((juxt identity #(rotate-r 1 %) #(rotate 1 %) #(rotate 2 %)) (keys dirs-map)))))

(defn move-toward [dir [y x]]
  (let [[dy dx] (dirs-map dir)]
    [(+ y dy) (+ x dx)]))

(defn starting-pos [lines] (vec (repeat 2 (/ (dec (count lines)) 2))))

(defn lines->sparse-grid [lines]
  (->> lines
       (map-indexed (fn [y row]         ; Get matrix pairs of coord -> char : [[1 3] \#]
                      (map-indexed (fn [x ch] [[y x] ch]) row)))
       (apply concat)                   ; Flatten the nested lists from inner map-indexed
       (filter #(= \# (second %)))      ; Keep only pairs matched to a pound (infected)
       (apply concat)                   ; Flatten again...
       (apply hash-map)))               ; Final map of {[0 0] \#, [0 3] \#, [1 4], \# ...}

(defn infected? [ch] (= ch \#))

(def genesis-fns "First generation (Pandemic 1)"
  {:was-infected? (complement infected?)
   :turn-with     (fn [node-state] (if (infected? node-state) :right :left))
   :infect-with   (fn [prev-node-state grid pos]
                    (if (infected? prev-node-state)
                      (dissoc grid pos)
                      (assoc grid pos \#)))})

(def evolved-fns "Second generation (Pandemic 2)"
  {:was-infected? (fn [prev-node-state] (= \W prev-node-state))
   :turn-with     (fn [node-state]
                    (case node-state
                      \W :forward
                      \# :right
                      \F :reverse
                      nil :left))
   :infect-with   (fn [prev-node-state grid pos]
                    (case prev-node-state
                      nil (assoc grid pos \W)
                      \W  (assoc grid pos \#)
                      \#  (assoc grid pos \F)
                      \F  (dissoc grid pos)))})

;; The 'grid' here isn't a 2d array, but a hash-map of coordinate point -> character.
;; This lets the grid expand arbitrarily in any direction without needing to resize
;; and shift arrays around. This is why init-pos needs to be supplied separately,
;; because the initial grid doesn't have a 'center', because uninfected nodes aren't
;; tracked.
(defn bursts
  [strain-fns init-pos init-grid iterations]
  (loop [i 0
         infections 0
         grid init-grid
         {:keys [pos dir] :as carrier} {:pos init-pos
                                        :dir :n}]
    (if (>= i iterations)
      infections
      (let [node-state     (get grid pos)
            new-dir        (get-in turns-map ; Turn right if node is infected, else left
                                   [dir ((:turn-with strain-fns) node-state)])
            new-pos        (move-toward new-dir pos)]
        (recur
         (inc i)
         (+ infections (if ((:was-infected? strain-fns) node-state) 1 0))
         ((:infect-with strain-fns) node-state grid pos)
         (assoc carrier :pos new-pos :dir new-dir))))))

(defn play-pandemic
  ([iterations input] (play-pandemic genesis-fns iterations input))
  ([strain-fns iterations input]
   (let [cleaned (-> input (.trim) str/split-lines)]
     (bursts strain-fns (starting-pos cleaned) (lines->sparse-grid cleaned) iterations))))

(def play-pandemic-2 (partial play-pandemic evolved-fns))


(defn part1 [] (->> (input-for-day "22") (play-pandemic   10000)))    ; => 5552
(defn part2 [] (->> (input-for-day "22") (play-pandemic-2 10000000))) ; => 2511527
;; Sluggish, execution time is 28 seconds.

(with-test
  (def example "
..#
#..
...")

  (is (= (->> example (play-pandemic   10000))    5587))
  (is (= (->> example (play-pandemic-2 100))      26))
  (is (= (->> example (play-pandemic-2 10000000)) 2511944)))

