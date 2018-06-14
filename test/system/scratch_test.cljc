(ns system.scratch-test
  (:require
   [system.core :refer :all]
   [clojure.core.match :refer [match]]
   [clojure.test :as t]
   [clojure.core.async :as as]))

(def sum-fn
  (fn [list]
    (match list
           [a] a
           [a b] (+ a b))))

(def double-fn
  (fn [v] (* 2 v)))

(t/deftest scratch
  (let [sub-graph (connect [[:in (node inc) :out]])
        scheme [
                [[:in :feedback] first-or-all (node sum-fn) (node double-fn) mult :out1 :feedback]
                [:out1 :out]
                [:out1 sub-graph :out2]
                ]
        graph (connect scheme)]

    (as/put! (:in graph) 5)

    (t/is (= 10 (<!!? (:out graph))))
    (t/is (= 11 (<!!? (:out2 graph))))

    (as/put! (:in graph) 5)

    (t/is (= 30 (<!!? (:out graph))))
    (t/is (= 31 (<!!? (:out2 graph))))

    (as/put! (:in graph) 5)

    (t/is (= 70 (<!!? (:out graph))))
    (t/is (= 71 (<!!? (:out2 graph))))))
