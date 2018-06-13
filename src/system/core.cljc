(ns system.core
  (:require [clojure.core.async :as as]))

(defn node [body-fn]
  (let [in (as/chan)
        out (as/chan)]
    (as/go-loop []
      (->> in
           (as/<!)
           (body-fn)
           (as/>! out))
      (recur))
    {:in in :out out}))

(defn all [init-chs]
  (let [out (as/chan)]
    (as/go-loop [chs init-chs vls []]
      (let [[vl ch] (as/alts! chs :priority true)
            rest-chs (remove #(= ch %) chs)
            acc-vls (conj vls vl)]
        (if (empty? rest-chs)
          (do (as/>! out acc-vls)
              (recur init-chs []))
          (recur rest-chs acc-vls))))
    out))

(defn mult [& args]
 (apply as/mult args))

(defn tap [mc]
  (let [out (as/chan)]
    (as/tap mc out)
    out))

(defn connect0 [{:keys [prev graph] :as acc} x]
  (cond
    ;; assume vector with keywords can only appear first
    (vector? x) (let [new-graph (reduce #(assoc %1 %2 (as/chan)) graph x)
                      chs (map (partial get new-graph) x)]
                  {:prev chs  :graph new-graph})
    (map? x) (do (as/pipe prev (:in x))
                 {:prev (:out x) :graph graph})
    ;; is first in row?
    (and (nil? prev) (keyword? x)) (if (contains? graph x)
                                     {:prev (get graph x) :graph graph}
                                     (let [c (as/chan)]
                                       {:prev c :graph (assoc graph x c)}))
    (keyword? x) (if (contains? graph x)
                   {:prev (get graph x) :graph graph}
                   {:prev prev :graph (assoc graph x prev)})
    (fn? x) {:prev (x prev) :graph graph}
    :else acc))

(defn connect [rows]
  (:graph (reduce
           #(reduce connect0 (assoc %1 :prev nil) %2)
           {:prev nil :graph {}}
           rows)))
