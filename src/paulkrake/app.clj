(ns paulkrake.app
  (:require [com.stuartsierra.component :as component]))

(defn example-system [config-options]
  (let [{:keys [host port]} config-options]
    (component/system-map 
     ;:db nil ;;(new-database host port)
     ;:scheduler nil;;(new-scheduler)
     ;:app    (component/using (example-component config-options)
     ;{:database  :db               :scheduler :scheduler})
     )))
