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
  "Liefert eine Map die den Schlüssel [s t] auf eine paulkrake.mwua->State abbildet. Der Spieltag ([sa ta] - n) wird auf den init-state abgebildet"
  ([sa ta n]
   (let [[sl tl] (dc/spieltag-add sa ta (* -1 n))
         state (read-state sl tl)]
     (expert-distribution sa ta n state)))
  ([sa ta n init-state]
   (let [[sl tl] (dc/spieltag-add sa ta (* -1 n))
         [sn tn] (dc/spieltag-add sa ta 1)]
     (loop [state init-state
            sps (dc/range-spieltage sn tn n)
            spieltag2state {[sl tl] state}]
       (let [[s t] (first sps)
             rest_sps (rest sps)
             new_state (m/step state (dc/spieltag s t)
                               (cost-fn-factory s t))
             new_spieltag2state (assoc spieltag2state [s t] new_state)]
         (if (empty? rest_sps)
           new_spieltag2state
           (recur new_state
                  rest_sps
                  new_spieltag2state
                  )))))) )

(defn expert-predict [s t]
  (let [[sn tn] (dc/spieltag-add s t -1)
        state_last (read-state sn tn)
        i (m/draw state_last)
        n (nth (:experts state_last) i)]
    (println n)
    (g/predict-result s t n)))
