(ns user
  (:require [com.stuartsierra.component :as component]
            [clojure.tools.namespace.repl :refer (refresh)]
            [paulkrake.app :as app ]
           )
  (:use [paulkrake.glicko2 ]
        [paulkrake.spielplan]
        [paulkrake.score]
        [paulkrake.bulibox]
        [paulkrake.datacenter]
        [paulkrake.data]
        [paulkrake.statistik]
        [paulkrake.predict]))

;(predict-games (adjust-ratings (ratings1415) (spieltag 1)) (spieltag 2))


(as-> (range 8) x
            (map #(read-rating-data 1415 %) x)
            (map #(get-in % ["Hamburger SV" :angriff :rating]) x)
            )

(def system nil)

(defn init []
  (alter-var-root #'system (constantly (app/example-system {:host "dbhost.com" :port 123})))
  )

(defn start []
  (alter-var-root #'system component/start))

(defn stop []
  (alter-var-root #'system
    (fn [s] (when s (component/stop s)))))

(defn go []
  (init)
  (start))

(defn reset []
  (stop)
  (refresh :after 'user/go))
