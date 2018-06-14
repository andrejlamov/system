(ns system.core
  (:require [clojure.core.async :as as]
            [clojure.core.match :refer [match]]))

(defn node
  ([body-fn] (node 1 body-fn))
  ([n body-fn]
   (let [in (as/chan)
         out (as/chan n)]
     (dotimes [_ n]
       (as/go-loop []
         (->> in
              (as/<!)
              (body-fn)
              (as/>! out))
         (recur)))
       {:in in :out out})))

(defn first-or-all [init-chs]
  (let [out (as/chan)]
    (as/go-loop [chs init-chs vls []]
      (let [[vl ch] (as/alts! chs :priority true)
            rest-chs (remove #(= ch %) chs)
            acc-vls (conj vls vl)]
        (if (or (= (first init-chs) ch) (empty? rest-chs))
          (do (as/>! out acc-vls)
              (recur init-chs []))
          (recur rest-chs acc-vls))))
    out))

(defn <!!?
  ([chan]
   (<!!? chan 1000))
  ([chan milliseconds]
   (let [timeout (as/timeout milliseconds)
         [value port] (as/alts!! [chan timeout])]
     (if (= chan port)
       value
       nil))))

(defn mult [& args]
 (apply as/mult args))

(declare connect connect-nodes connect-channels)

(defn connect [rows]
  (:graph (reduce
           #(reduce connect-nodes (assoc %1 :prev nil) %2)
           {:prev nil :graph {}}
           rows)))

(defn connect-nodes [{:keys [prev graph] :as acc} x]
  (match [prev x]

         [_   [[& xs]]]
         (connect-nodes acc (connect x))

         [nil [& xs]]
         (let [new-graph (reduce #(assoc %1 %2 (as/chan)) graph x)
               chs (map (partial get new-graph) x)]
           {:prev chs  :graph new-graph})

         [nil (_ :guard keyword?)]
         (if (contains? graph x)
           (let [in (get graph x)]
             {:prev (connect-channels in)  :graph graph})
           (let [c (as/chan)]
             {:prev c :graph (assoc graph x c)}))

         [_ (_ :guard keyword?)]
         (if (contains? graph x)
           (let [out (get graph x)]
             {:prev (connect-channels prev out) :graph graph})
           {:prev prev :graph (assoc graph x prev)})

         [_ (_ :guard map?)]
         (do (connect-channels prev (:in x))
             {:prev (:out x) :graph graph})

         [_ (_ :guard fn?)]
         {:prev (x prev) :graph graph}

         :else acc))

(defn connect-channels
  ([in] (connect-channels in (as/chan)))
  ([in out]
   (if (satisfies? clojure.core.async/Mult in)
     (as/tap in out)
     (as/pipe in out))
   out))


