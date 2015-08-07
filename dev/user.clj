(ns user
  (:require [com.stuartsierra.component :as component]
            [clojure.tools.namespace.repl :refer (refresh)]
            [paulkrake.app :as app ]
            [net.cgrand.enlive-html :as html]
            [clojure.data.json :as json]
            ;;[svm.core :as svm]
           )
  (:use [paulkrake.glicko2 ]
        [paulkrake.spielplan]
        [paulkrake.score]
        [paulkrake.bulibox]
        [paulkrake.datacenter]
        [paulkrake.data]
        [paulkrake.ml]
        [paulkrake.statistik]
        [paulkrake.predict]
        [paulkrake.goals]
        [paulkrake.shots]
        [paulkrake.goals-per-shots]))

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
