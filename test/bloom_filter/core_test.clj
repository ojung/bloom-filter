(ns bloom-filter.core-test
  (:require [clojure.test :refer :all]
            [clojure.test.check.clojure-test :as clj-test]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [bloom-filter.core :refer :all]))

(declare fill-filter try-lookup test-false-positives)

(clj-test/defspec can-lookup 1000
  (prop/for-all [s (gen/not-empty gen/string-ascii)]
                (do (reset) (add s) (lookup s))))

(clj-test/defspec false-positive-ratio-in-delta 100
  (do
    (reset 10000)
    (let [strings (fill-filter 100)
          counter (atom 0)]
      (prop/for-all [string (gen/not-empty gen/string-ascii)]
                    (test-false-positives strings string counter)))))

(defn- fill-filter [num-entries]
  (let [strings (gen/sample (gen/not-empty gen/string-ascii) num-entries)]
    (for [string strings] (add string))))

(defn- test-false-positives [filter-content string counter]
  (if (= (.indexOf filter-content string) -1)
    (do
      (try-lookup string counter)
      (< @counter 10))
    true))

(defn- try-lookup [string counter]
  (when (lookup string)
    (swap! counter inc)))
