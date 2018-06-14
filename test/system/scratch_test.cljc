(ns system.scratch-test
  (:require
   [system.core :as s]
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
  (let [scheme-a [[:in (s/node 100 sum-fn) (s/node 100 double-fn) :out]]

        scheme-b [[:in (s/node inc) :out]]
        connected-b (s/connect scheme-b)

        scheme [
                [[:in :feedback] s/first-or-all scheme-a s/mult :out1 :feedback]
                [:out1 :out]
                [:out1 connected-b :out2]
                ]
        graph (s/connect scheme)]

    (as/put! (:in graph) 5)

    (t/is (= 10 (s/<!!? (:out graph))))
    (t/is (= 11 (s/<!!? (:out2 graph))))

    (as/put! (:in graph) 5)

    (t/is (= 30 (s/<!!? (:out graph))))
    (t/is (= 31 (s/<!!? (:out2 graph))))

    (as/put! (:in graph) 5)

    (t/is (= 70 (s/<!!? (:out graph))))
    (t/is (= 71 (s/<!!? (:out2 graph))))))
