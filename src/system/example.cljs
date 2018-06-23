(ns system.example
  (:require [system.core :as s]
            [clojure.core.async :as as]
            [clojure.core.match :refer [match]]
            [rum.core :as rum]))

(enable-console-print!)

(rum/defc rumc [ui] ui)

;; when-apply
(defn wa [f & args]
  (when f
    (apply f args)))

(defn root
  [{:keys [counter-number counter-buttons system-tick]
    :as all} event-ch]
  [:div [(wa counter-number) (wa counter-buttons event-ch) (wa system-tick)]])

(def state-merger
  (s/reducer "state-merger" merge))

(def hiccup-mounter
  (s/async-reducer "hiccup-mounter"
                   (fn [acc hiccup event-ch]
                     (let [acc1 (merge acc hiccup)]
                       (rum/mount (rumc
                                   (root acc1 event-ch))
                                  (. js/document getElementById "root"))
                       acc1))))

(defn logger [line]
  (s/mapper "logger" (fn [v] (println line v) v)))

(def counter-materializer
  (s/mapper "counter/materializer" :counter))

(def system-materializer
  (s/mapper "system/materializer" :system))

(def counter-updater
  (s/mapper "counter/updater"
            (fn [event]
              (match event
                     [:inc :counter _ m] {:counter (update m :number inc)}
                     [:dec :counter _ m] {:counter (update m :number dec)}
                     _ {}))))

(def counter-buttons
  (s/mapper "counter/hiccuper/buttons"
            (fn [{:keys [number] :as materialized}]
              {:counter-buttons (fn [event-ch]
                                  [:div
                                   [:button
                                    {:on-click #(as/put! event-ch [:dec :counter nil materialized])}
                                    "decrement"]
                                   [:button
                                    {:on-click #(as/put! event-ch [:inc :counter nil materialized])}
                                    "increment"]])})))

(def counter-number
  (s/mapper "counter/hiccuper/number"
            (fn [{:keys [number] :as materialized}]
              {:counter-number (fn []
                                 [:h3 (str "counter: " number)])})))

(def tick-updater
  (s/mapper "system/updater" (fn [event]
                               (match event
                                      [:tick :system _ m] m
                                      _ {}))))

(def ticker-signal
  (s/mapper "system/hiccuper/tick-signal"
            (fn [{:keys [tick]}]
              {:system-tick (fn [] [:h1 (if tick "high" "low")])})))

(def ticker
  (s/source "ticker" (fn [in out]
                       (as/go-loop [high? true]
                         (let [[v ch] (as/alts! [(as/timeout 1000) in])]
                           (if (not (= ch in))
                             (do
                               (as/>! out [:tick :system high? {:system {:tick high?}}])
                               (recur (not high?)))
                             (println "ticker shutdown")))))))

(defonce graph (atom {}))

(when (not (empty? @graph))
 (as/close! (:state.in @graph)))

(def scheme
  [
   [:state.in state-merger s/mult :state.out]
   [:mounter.in hiccup-mounter :event.in]
   [:event.in s/mult :event.out] ;format: [event-name origin value materialized]
   [:event.out tick-updater :state.in]
   [:event.out counter-updater :state.in]
   [:state.out counter-materializer s/mult :materializer.counter]
   [:materializer.counter counter-number :mounter.in]
   [:materializer.counter counter-buttons :mounter.in]
   [:state.out system-materializer ticker-signal :mounter.in]
   [:ticker.in ticker :event.in]])

(reset! graph (s/connect scheme))

;; initial state
(as/put! (:state.in @graph) {:counter {:number 0 } :system {:tick nil}})

(defn on-js-reload []
  (println "reload!"))
