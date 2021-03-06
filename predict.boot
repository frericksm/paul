#!/usr/bin/env boot

(set-env!
 :source-paths #{"src"}
 :dependencies '[[org.clojure/clojure "1.9.0-alpha14"]
                 [com.stuartsierra/component "0.2.1"]
                 [enlive "1.1.5"]
                 [incanter "1.5.5"]
                 [org.clojure/data.json "0.2.5"]
                 ;;[svm-clj "0.1.3"]
                 ])

(require '[paulkrake.predict])
(require '[paulkrake.datacenter])
(require '[paulkrake.goals])
(require '[clojure.pprint])
(require '[boot.cli :refer [defclifn]])

(defclifn -main 
  [s saison   VAL  int   "Saison auf den Wert VAL. Z.B 1415 für die Saision 2014/2015"
   t spieltag VAL  int   "Spieltag auf den Wert VAL. Z.B 17 für den 17. Spieltag"]
   z zurueck  VAL  int   "Greife auf die letzten z Spieltage bei der Modellbildung für die Vorhersage zurück"
   (let [results (paulkrake.predict/predict-result s t z)]
    (doseq [r results]
      (println r))))
       

