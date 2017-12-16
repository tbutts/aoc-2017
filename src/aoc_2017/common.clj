(ns aoc-2017.common
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn input-for-day [day] (-> (str day "/input") io/resource slurp))

(defn input-filename [day] (str "src/aoc-2017/" day "/input"))

(defn print-ans
  [day part solver]
  (println (str day "." part) "answer:" (solver)))

(defn run
  [day solvers]
  (dorun (map-indexed
          #(print-ans day (inc %1) %2) solvers)))

;; Input Parsing
(defn char-seq
  "Returns each char of text from rdr as a lazy sequence of chars."
  [^java.io.BufferedReader rdr]
  (when-let [ch (let [c (.read rdr)] (when (not= c -1) (char c)))]
    (cons ch (lazy-seq (char-seq rdr)))))

(defn no-spaces [s] (str/replace s #"\s+" ""))
(defn split-commas [s] (str/split s #","))
(defn parse-int [s] (java.lang.Integer/parseInt s 10))

;; Math
(defn abs [n] (max n (- n)))


