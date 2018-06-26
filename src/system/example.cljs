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

(defn mount-hiccup [acc hiccup event-ch]
  (let [acc1 (merge acc hiccup)]
    (rum/mount (rumc
                (root acc1 event-ch))
               (. js/document getElementById "root"))
    acc1))

(defn update-counter [event]
  (match event
         [:inc :counter _ m] {:counter (update m :number inc)}
         [:dec :counter _ m] {:counter (update m :number dec)}
         _ {}))

(defn buttons-hiccup [{:keys [number] :as materialized}]
  {:counter-buttons (fn [event-ch]
                      [:div
                       [:button
                        {:on-click #(as/put! event-ch [:dec :counter nil materialized])}
                        "decrement"]
                       [:button
                        {:on-click #(as/put! event-ch [:inc :counter nil materialized])}
                        "increment"]])})

(defn counter-hiccup [{:keys [number] :as materialized}]
  {:counter-number (fn []
                     [:h3 (str "counter: " number)])})

(defn update-system [event]
  (match event
         [:tick :system _ m] m
         _ {}))

(defn ticker-signal-hiccup [{:keys [tick]}]
  {:system-tick (fn [] [:h1 (if tick "high" "low")])})

(defn ticker [in out]
  (as/go-loop [high? true]
    (let [[v ch] (as/alts! [(as/timeout 1000) in])]
      (if (not (= ch in))
        (do
          (as/>! out [:tick :system high? {:system {:tick high?}}])
          (recur (not high?)))
        (println "ticker shutdown")))))

(defonce graph (atom {}))
(s/close @graph)

(defn scheme []
  (let [
        state-merger (s/reducer "state-merger" merge)
        hiccup-mounter (s/async-reducer "hiccup-mounter" mount-hiccup)
        counter-materializer (s/mapper "counter-materializer" :counter)
        counter-hiccup (s/mapper "counter-hiccup" counter-hiccup)
        counter-updater (s/mapper "counter-updater" update-counter)
        buttons-hiccup (s/mapper "buttons-hiccup" buttons-hiccup)
        system-materializer (s/mapper "system-materializer" :system)
        system-updater (s/mapper "system-updater" update-system)
        ticker-signal-hiccup (s/mapper "ticker-signal-hiccup" ticker-signal-hiccup)
        ticker (s/source "ticker" ticker)

        schema [
                [state-merger s/mult "state.mult"]
                ["event.in" s/mult "event.mult"]
                [hiccup-mounter "event.in"]
                ["event.mult" system-updater state-merger]
                ["event.mult" counter-updater state-merger]

                ["state.mult" counter-materializer s/mult "counter.mult"]
                ["counter.mult" counter-hiccup hiccup-mounter]
                ["counter.mult" buttons-hiccup hiccup-mounter]
                ["state.mult" system-materializer ticker-signal-hiccup hiccup-mounter]
                [ticker "event.in"]]
        ]
    ;; initial state
    (as/put! (:in state-merger) {:counter {:number 0 } :system {:tick nil}})

    schema))

(reset! graph (s/connect (scheme)))

@graph

(defn on-js-reload []
  (println "reload!"))
