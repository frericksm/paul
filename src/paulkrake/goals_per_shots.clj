(ns paulkrake.goals-per-shots
  (:require [paulkrake.score :as s]
            [paulkrake.predict :as p]
            [paulkrake.spielplan :as sp]
            [paulkrake.datacenter :as dc]
            [incanter.distributions :as d]))

(defn gps-to-score-fn
  "Maps number of gps to a score value from [0 1].
   The mean of gps per game is 0.1177"
  [gps]
  ;;(d/cdf (d/poisson-distribution 11.77) gps)
  gps
  )

(defn score-to-gps-fn [score]
  (as-> (range -1 100) x
        (partition 2 1 x)
        (filter (fn [[a b]]
                  (and (<= (gps-to-score-fn a) score )
                       (<= score (gps-to-score-fn b)))) x)
        (map (fn [[a b]]
               (+ a (*  (/ (- score (gps-to-score-fn a)) 
                           (- (gps-to-score-fn b) (gps-to-score-fn a))) 
                        (- b a)))) x)
        (first x)))

(defn new-rating-gps [data games]
  (s/new-rating data games gps-to-score-fn))

(defn gps-data
  ([saison spieltag-nr-von spieltag-nr-bis]
     (as-> (s/initial-rating-data (s/vereine (dc/spieltag saison 1))) x
           (reduce (fn [a i]
                     (new-rating-gps a
                                     (sp/spieltag-treffer-pro-shots saison i)))
                   x (range spieltag-nr-von spieltag-nr-bis))))
  ([saison spieltag-nr-bis]
     (gps-data saison 1 spieltag-nr-bis)))

