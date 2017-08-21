(ns paulkrake.expert-selection
  (:require [paulkrake.datacenter :as dc]
            [paulkrake.goals :as g]
            [clojure.edn]
            [paulkrake.crossval :as cv]
            [paulkrake.mwua :as m]))

(defn expert-state-file [s t]
  (clojure.java.io/file (format "resources/expert-state-%s-%s.txt"
                                (str s) (str t))))

(defn write-state [saison spieltag expert-state]
  (with-open [wtr (clojure.java.io/writer (expert-state-file saison spieltag))] 
    (as-> (select-keys expert-state [:weights :experts :learning-rate]) x
      (pr-str x)   
      (.write wtr x))))


(defn read-state [saison spieltag]
  (let [f (expert-state-file saison spieltag)]
    (if (not (.exists f))
      (throw (RuntimeException. (str "File existiert nicht: "
                                     (.getAbsolutePath f) "f"))))
    (as-> (slurp (expert-state-file saison spieltag)) x
      (clojure.edn/read-string x)
      (m/->State (:experts x)
                 (:weights x)
                 (:learning-rate x)
                 (java.security.SecureRandom.)))))

(defn cost-fn-factory [s t]
  (fn cost-fn [n res]
    (- 1 (/ (* -1  (cv/measure (dc/spieltag s t)
                               (g/predict-result s t n)
                               cv/metric-kickerpoints))
            36.0))))

#_(def experts [5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22])
(def experts [12 13 14 15 16 17 18 19 20 21 22])

(defn init-state [experts]
  (m/->State  experts (vec (repeat (count experts) 1)) 0.5 (java.security.SecureRandom.)))

(defn expert-distribution
  "Nimmt eine Liste von Spielttage als Parameter spielttage.
  In der Reihenfolge der Spieltage in der Liste wird die Entwicklung des init-state berechnet"
  [spieltage init-state]
  (loop [state init-state
         sps spieltage]
    (let [[s t] (first sps)
          rest_sps (rest sps)
          new_state (m/step state (dc/spieltag s t)
                            (cost-fn-factory s t))]
      (if (empty? rest_sps)
        new_state
        (recur new_state
               rest_sps )))))

(defn expert-predict [s t exp-state]
  (let [i (m/draw exp-state)
        n (nth (:experts exp-state) i)]
    (println n)
    (g/predict-result s t n)))



(defn best-experts [s t]
  (as-> (dc/range-spieltage s t 1200) x
    (filter (fn [[s ta]] (= ta t)) x)
    (expert-distribution x (init-state [5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25] ))
    (m/experts-sorted-by-weight x)))

(comment 

(doseq [[[s t] v] (expert-distribution 1617 31 1 )]
        (write-state s t v ))

)
