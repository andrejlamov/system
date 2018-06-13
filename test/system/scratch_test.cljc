(ns system.scratch-test
  (:require
   [system.core :refer :all]
   [clojure.test :as t]
   [clojure.core.async :as as]))

(def sum-fn
  (fn [[a b]]
    (let [r (+ a b)]
      '(println "sum. args:" a b "return: " r)
      r)))

(def double-fn
  (fn [v]
    (let [r (* 2 v)]
      '(println "double. args:" v "return: " r)
      r)))

(t/deftest scratch
  (let [scheme [
                [[:in1 :in2] all (node sum-fn) (node double-fn) mult :out1]
                [:out1 tap (node inc) :out3]
                [:out1 tap (node dec) :out4]
                ]
        graph (connect scheme)]
    (as/put! (:in1 graph) 3)
    (as/put! (:in2 graph) 2)
    (t/is (= 11 (as/<!! (:out3 graph))))
    (t/is (= 9 (as/<!! (:out4 graph))))))
