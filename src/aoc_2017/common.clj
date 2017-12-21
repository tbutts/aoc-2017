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

;; Higher Order Fns
(defn flip [f] #(apply f (reverse %&)))

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

(def split-flop (flip str/split))
(def replace-flop (flip str/replace))

(defn no-spaces [s] (str/replace s #"\s+" ""))
(defn split-spaces [s] (str/split s #"\s+"))
(defn split-commas [s] (str/split s #","))
(defn parse-int
  ([s] (Integer/parseInt s 10))
  ([s radix] (Integer/parseInt s radix)))

(defn hexify [bytes] (str/join (map (partial format "%02x") bytes)))

(defn hexdigit->binstr
  "Convert a 4-digit hex string into binary string representation.
  E.g. 'a0c2' becomes '1010000011000010'"
  [hex-digit]
  (->> (parse-int hex-digit 16)
       (Integer/toBinaryString)
       (format "%16s")
       (#(str/replace % " " "0"))))
(defn hex->binstr [hex] (->> (re-seq #".{4}" hex) (map hexdigit->binstr) str/join))

(def lowercase-alphabet (map #(char (+ (byte \a) %)) (range 26)))

;; Math
(defn abs [n] (max n (- n)))

;; Search
(defn bfs
  [flood-fn pred graph root-vertex]
  (loop [queue [root-vertex]
         visited #{}]
    (if (empty? queue)
      visited
      (recur (remove (partial pred graph visited) (into #{} (mapcat flood-fn queue)))
             (into visited queue)))))

;; Other
(defn map-kv [f coll]
  (reduce-kv (fn [m k v] (assoc m k (f v))) (empty coll) coll))

(defn rotate [n coll]
  (let [[head tail] (split-at n coll)]
    (concat tail head)))

(defn rotate-r [n coll]
  (let [[head tail] (split-at (- (count coll) n) coll)]
    (concat tail head)))

(defn swap-idx [coll i j]
  (assoc coll i (coll j) j (coll i)))

