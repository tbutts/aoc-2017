(ns aoc-2017.common
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (java.io BufferedReader StringReader)))

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
(defn read-char
  "Reads a single byte, returning it as a character or nil at EOF"
  [^BufferedReader rdr]
  (let [c (.read rdr)]
    (when-not (= c -1) (char c))))

(defn char-seq
  "Returns each char of text from rdr as a lazy sequence of chars."
  [^BufferedReader rdr]
  (when-let [ch (read-char rdr)]
    (cons ch (lazy-seq (char-seq rdr)))))

(defn comma-seq
  "Returns a lazy sequence of text between commas. The commas are removed."
  [^BufferedReader rdr]
  (when-some [text (->> (char-seq rdr) (take-while (partial not= \,)) str/join not-empty)]
    (cons text (lazy-seq (comma-seq rdr)))))

(defn bad-comma-seq
  "Returns a lazy sequence of text between commas. The commas are removed."
  [^BufferedReader rdr]
  (loop [text []]
    (if-let [ch (read-char rdr)]
      (if (= ch \,)
        (cons (str/join text) (lazy-seq (bad-comma-seq rdr)))
        (recur (conj text ch)))
      (str/join text))))

(defn no-spaces [s] (str/replace s #"\s+" ""))
(defn split-spaces [s] (str/split s #"\s+"))
(defn split-commas [s] (str/split s #","))
(defn parse-int [s] (java.lang.Integer/parseInt s 10))

(defn hexify [bytes] (str/join (map (partial format "%02x") bytes)))

;; Math
(defn abs [n] (max n (- n)))

;; Other
(defn map-kv [f coll]
  (reduce-kv (fn [m k v] (assoc m k (f v))) (empty coll) coll))

