(ns system.core
  (:require [clojure.core.async :as as]
            [clojure.core.match :refer [match]]))

(defn async-reducer
  [body-fn]
  (let [in (as/chan)
        out (as/chan)]
    (as/go-loop [acc0 nil]
      (let [d (as/<! in)
            acc1 (body-fn acc0 d out)]
        (recur acc1)))
    {:in in :out out}))

(defn reducer [body-fn]
  (let [in (as/chan)
        out (as/chan)]
    (as/go-loop [acc0 nil]
      (let [d (as/<! in)
            acc1 (body-fn acc0 d)]
        (as/>! out acc1)
        (recur acc1)))
    {:in in :out out}))

(defn mapper [body-fn]
  (let [in (as/chan)
        out (as/chan)]
    (as/go-loop []
      (->> in
           (as/<!)
           (body-fn)
           (as/>! out))
      (recur))
    {:in in :out out}))

(defn async-mapper [body-fn]
  (let [in (as/chan)
        out (as/chan)]
    (as/go-loop []
      (let [d (as/<! in)]
        (body-fn d out))
      (recur))
    {:in in :out out}))

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


