(ns system.core
  (:require [clojure.core.async :as as]
            [clojure.core.match :refer [match]]))

(defn async-reducer
  ([body-fn] (async-reducer nil body-fn))
  ([name body-fn]
   (let [in (as/chan)
         out (as/chan)]
     (as/go-loop [acc0 nil]
       (if-some [d (as/<! in)]
         (let [acc1 (body-fn acc0 d out)]
           (recur acc1))
         (do
           (println name "shutdown")
           (as/close! out))))
     {:in in :out out :name name})))

(defn reducer
  ([body-fn] (reducer nil body-fn))
  ([name body-fn]
   (let [in (as/chan)
         out (as/chan)]
     (as/go-loop [acc0 nil]
       (if-some [d (as/<! in)]
         (let [acc1 (body-fn acc0 d)]
           (as/>! out acc1)
           (recur acc1))
         (do
           (println name "shutdown")
           (as/close! out))))
     {:in in :out out :name name})))

(defn mapper
  ([body-fn] (mapper nil body-fn))
  ([name body-fn]
   (let [in (as/chan)
         out (as/chan)]
     (as/go-loop []
       (if-some [d (as/<! in)]
         (do
           (as/>! out (body-fn d))
           (recur))
         (do
           (println name "shutdown")
           (as/close! out))))
     {:in in :out out :name name})))

(defn async-mapper
  ([body-fn] (async-mapper nil body-fn))
  ([name body-fn]
   (let [in (as/chan)
         out (as/chan)]
     (as/go-loop []
       (if-some [d (as/<! in)]
         (do
           (body-fn d out)
           (recur))
         (do
           (println name "shutdown")
           (as/close! out))))
     {:in in :out out :name name})))

(defn source
  ([body-fn] (async-mapper nil body-fn))
  ([name body-fn]
   (let [out (as/chan)
         in (as/chan)]
     (body-fn in out)
     {:in in :out out :name name})))

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
           (fn [acc x]
             (println " ")
             (reduce connect-nodes (assoc acc :prev nil) x))
           {:prev nil :graph {} :vis []}
           rows)))

(defn name [node]
  (cond (keyword? node) node
        (map? node) (:name node)
        :else "channel"))

(defn connect-nodes [{:keys [prev graph vis] :as acc} x]
  (let [res (match [prev x]
                   [nil (_ :guard map?)]
                   (do
                     (print (str \" (:name x) \") "->")
                     {:prev (:out x) :graph (assoc graph x (:in x))})

                   [nil (_ :guard #(or (keyword? %) (string? %)))]
                   (do
                     (print (str \" x \") "->")
                     (if (contains? graph x)
                       (let [in (get graph x)]
                         {:prev (connect-channels in)  :graph graph})
                       (let [c (as/chan)]
                         {:prev c :graph (assoc graph x c)})))

                   [_ (_ :guard #(or (keyword? %) (string? %)))]
                   (do (print (str \" x \")  "->")
                       (if (contains? graph x)
                         (let [out (get graph x)]
                           {:prev (connect-channels prev out) :graph graph})
                         {:prev prev :graph (assoc graph x prev)}))

                   [_ (_ :guard map?)]
                   (do
                     (print (str \" (:name x) \") "->")
                     (connect-channels prev (:in x))
                     {:prev (:out x) :graph (assoc graph x (:out x))})

                   [_ (_ :guard fn?)]
                   {:prev (x prev) :graph graph}

                   :else acc)]
    res))

(defn connect-channels
  ([in] (connect-channels in (as/chan)))
  ([in out]
   (if (satisfies? clojure.core.async/Mult in)
     (as/tap in out)
     (as/pipe in out))))

(defn close [graph]
  (doseq [c (vals graph)]
    (try
      ;; TODO: Check if channel to avoid exception handling.
      (as/close! c)
      #?(:cljs (catch js/Object identity)
         :clj (catch Exception identity)))))

