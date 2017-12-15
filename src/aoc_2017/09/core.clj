(ns aoc-2017.09.core
  (:require [clojure.test :refer [are is with-test]]
            [clojure.string :as str]
            [clojure.java.io :as io])
  (:use [aoc-2017.common])
  (:import (java.io Reader
                    StringReader)))

(def base-state
  {:level 0
   :score 0
   :garbage? false
   :skip? false})

(defn step
  [state ^java.lang.Character ch]
  (let [new-state (partial assoc state)]
    (cond
      (:skip? state) (new-state :skip? false)
      (:garbage? state) (case ch
                          \! (new-state :skip? true)
                          \> (new-state :garbage? false)
                          state)
      :else (case ch
              \{ (update state :level inc)
              \} (-> state
                     (update :score + (:level state))
                     (update :level dec))
              \< (new-state :garbage? true)
              \> (new-state :garbage? false)
              state))))

(defn stream-process [^Reader res]
  (with-open [rdr (io/reader res)]
    (reduce step base-state (char-seq rdr))))

(def stream-process-score (comp :score stream-process))

;(defn sproc-str [s] (stream-process (StringReader. s)))
;(defn sproc-res [res] (stream-process (io/resource res)))

(with-test
  (def tests)

  (are [in expect] (= (stream-process-score (StringReader. in)) expect)
    "{}" 1
    "{{{}}}" 6
    "{{},{}}" 5
    "{{{},{},{{}}}}" 16
    "{<a>,<a>,<a>,<a>}" 1
    "{{<ab>},{<ab>},{<ab>},{<ab>}}" 9
    "{{<!!>},{<!!>},{<!!>},{<!!>}}" 9
    "{{<a!>},{<a!>},{<a!>},{<ab>}}" 3
    ))

(defn part1 [] (stream-process-score (io/resource "09/input")))
; => 21037
