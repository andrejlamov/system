(ns system.scratch-test
  (:require
   [system.core :as s]
   [clojure.core.match :refer [match]]
   [clojure.test :as t]
   [clojure.core.async :as as]))

(t/deftest simple-graph
  (let [scheme [[:in
                 (s/reducer (fn [acc v] (if (nil? acc) v (+ v acc))))
                 s/mult
                 :out1]
                [:out1 (s/mapper inc) :out2]
                [:out2 (s/async-mapper (fn [v ch] (as/put! ch v))) :out3]
                [:out1 :out]]

        graph (s/connect scheme)
        in (:in graph)
        out (:out graph)
        out2 (:out3 graph)
        ]

    (as/put! in 10)
    (t/is (= 10 (s/<!!? out)))
    (t/is (= 11 (s/<!!? out2)))

    (as/put! in 5)
    (t/is (= 15 (s/<!!? out)))

    (t/is (= 16 (s/<!!? out)))
    (t/is (= 17 (s/<!!? out2)))))

(t/deftest scheme-variations
  (let [a (s/mapper "a" inc)
        b (s/mapper "b" inc)
        c (s/mapper "c" inc)
        d (s/mapper "d" inc)
        e (s/mapper "e" inc)
        graph (s/connect [[:in a b]
                          [b d]
                          [d c s/mult :tap]
                          [:tap :out1]
                          [:tap e]
                          ])]
    (as/put! (:in graph) 10)
    (t/is (= 14 (s/<!!? (:out1 graph))))
    (t/is (= 15 (s/<!!? (get graph e))))))
