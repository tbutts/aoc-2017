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
   :chars 0
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
                          (update state :chars inc))
      :else (case ch
              \{ (update state :level inc)
              \} (-> state
                     (update :score + (:level state))
                     (update :level dec))
              \< (new-state :garbage? true)
              state))))

(defn stream-process [^Reader res]
  (with-open [rdr (io/reader res)]
    (reduce step base-state (char-seq rdr))))

(def stream-process-score (comp :score stream-process))
(def stream-process-chars (comp :chars stream-process))

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
    )

  (are [in expect] (= (stream-process-chars (StringReader. in)) expect)
    "<>" 0
    "<random characters>" 17
    "<<<<>" 3
    "<{!>}>" 2
    "<!!>" 0
    "<!!!>>" 0
    "<{o\"i!a,<{i<a>" 10))

(defn part1 [] (stream-process-score (io/resource "09/input")))
; => 21037

(defn part2 [] (stream-process-chars (io/resource "09/input")))

