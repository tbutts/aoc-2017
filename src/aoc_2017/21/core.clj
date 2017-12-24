(ns aoc-2017.21.core
  (:require
   [clojure.test :refer [are is with-test]]
   [clojure.string :as str])
  (:use [aoc-2017.common]))

;; Define a set of matrix manipulation functions
(defn transpose  [mat] (apply mapv vector mat)) ; Rows become columns, columns become rows
(defn flip-rl    [mat] (mapv reverse mat))             ; reverse rows
(defn rotate+90  [mat] (mapv reverse (transpose mat))) ; clockwise spin

(defn jumble-matrix-a-bunch
  "Generate all permutations of a matrix, to match against."
  [mat]
  (->> mat
       (iterate rotate+90)
       (take 4)
       (mapcat (juxt identity flip-rl))))

;; For sub-dividing a matrix into 2x2 or 3x3 submatricies
(defn block-size [len]
  (if (even? len) 2 3))

(defn split-mat
  "Splits the matrix into n-by-n subdivisions."
  [n mat]
  (if (= n (count mat))
    [mat]
    (->> mat
         (map (partial partition n))
         (partition n)
         (mapcat transpose))))

(defn concat-mat [parts]
  (let [total       (count (flatten parts))
        side-len    (count (flatten (first parts)))
        block-size  (long (Math/sqrt (/ total side-len)))]
    (->> parts
         (partition block-size)
         (mapcat transpose)
         (map flatten))))

(defn enhance™ [rules mat]
  (if-let [m (get rules mat)]
    m
    (throw (Exception. (str "No transform available for " mat)))))

(defn total-matrix-enhancement©
  [iters seed-mat rules]
  (let [enhancer (memoize (partial enhance™ rules))]
    (loop [i    0
           mat  seed-mat]
      (if (= i iters)
        mat
        (let [bsz (block-size (count mat))
              next-mat (->> mat (split-mat bsz) (map enhancer) concat-mat)]
          (recur (inc i) next-mat))))))

(defn count-on-bits [mat]
  (-> mat flatten frequencies (get \#)))

;; Input Parsing
(defn parse-rule [rule]
  (->> rule
       (split-flop #" => ")
       (mapv (comp (partial mapv vec) (partial split-flop #"/")))))

(defn explode-rules-list
  "Include all transformations of each key into the rules map"
  [old-rules]
  (reduce-kv (fn [rules match res]
               (reduce (fn [rs new-match] (assoc rs new-match res))
                       rules (jumble-matrix-a-bunch match)))
             {} old-rules))

(defn parse-input [in]
  (->> (.trim in)           ; Take the input "../.# => #.#/..#/.##\n<...>"
       str/split-lines      ; As individual lines
       (map parse-rule)     ; lines->pairs:  [[".." ".#"] ["#.#" "..#" ".##"]]
       (into {})            ; Slam the pairs into map key-values
       explode-rules-list   ; Include each rule's permutations
       ))

(defn print-rules-list [rules]
  (doseq [[init res] rules] (println init " => " res)))


;; Solution
(def base-pattern (-> ".#./..#/###" parse-rule first))

(defn generate-art [rules-input iters]
  (->> rules-input
       parse-input
       (total-matrix-enhancement© iters base-pattern)
       count-on-bits))

(defn part1 [] (-> (input-for-day "21") (generate-art 5)))  ; => 179
(defn part2 [] (-> (input-for-day "21") (generate-art 18))) ; => 2766750

(with-test
  (def example (.trim "
../.# => ##./#../...
.#./..#/### => #..#/..../..../#..#
"))

  (is (= 12 (-> example (generate-art 2))))

  ;; "Clojure supports identifiers with unicode!"
  (is (= 6
         (let [🎶🐙🎶  (fn [a] (+ a 2))]
           (🎶🐙🎶 4))))
  )

