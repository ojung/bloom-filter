(ns bloom-filter.core
  (:gen-class)
  (:import [clojure.lang Murmur3]))

(def filter-size (ref 10000))

(def bit-vector (ref (vec (repeat @filter-size false))))

(defn add [element]
  (let [murmur (mod (Murmur3/hashUnencodedChars element) @filter-size)
        java-hash (mod (.hashCode element) @filter-size)]
    (dosync (alter bit-vector #(assoc % murmur true java-hash true)) element)))

(defn lookup [element]
  (let [murmur (mod (Murmur3/hashUnencodedChars element) @filter-size)
        java-hash (mod (.hashCode element) @filter-size)]
    (and (nth @bit-vector murmur) (nth @bit-vector murmur))))

(defn reset
  ([] (reset 10000))
  ([size]
   (dosync
     (ref-set filter-size size)
     (ref-set bit-vector (vec (repeat @filter-size false))))))

(defn satuation [] (/ (count (filter identity @bit-vector)) 2))
