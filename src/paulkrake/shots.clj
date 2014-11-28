(ns paulkrake.shots
  (:require [paulkrake.score :as s]
            [paulkrake.predict :as p]
            [paulkrake.spielplan :as sp]
            [paulkrake.datacenter :as dc]
            [incanter.distributions :as d]))

(defn shots-to-score-fn
  "Maps number of shots to a score value from [0 1].
   The mean of shots per game is 12.81."
  [shots]
  (d/cdf (d/poisson-distribution 12.81) shots))

(defn score-to-shots-fn [score]
  (as-> (range -1 100) x
        (partition 2 1 x)
        (filter (fn [[a b]]
                  (and (<= (shots-to-score-fn a) score )
                       (<= score (shots-to-score-fn b)))) x)
        (map (fn [[a b]]
               (+ a (*  (/ (- score (shots-to-score-fn a)) 
                           (- (shots-to-score-fn b) (shots-to-score-fn a))) 
                        (- b a)))) x)
        (first x)))

(defn new-rating-shots [data games]
  (s/new-rating data games shots-to-score-fn))

(defn predict-shots
  ([data games]
     (predict-shots data games 1.0))
  ([data games faktor-sigma]
     (as-> games x
           (map (fn [[ h g]] (p/predict-single-game data h g faktor-sigma score-to-shots-fn)) x)
           (map (fn [[h g [hmin hmax] [gmin gmax]]] (format "%24s - %24s   [%.2f - %.2f] : [%.2f - %.2f]" h g hmin hmax gmin gmax)) x))))

(defn shots-data [saison spieltag-nr]
  (as-> (s/initial-rating-data (s/vereine (dc/spieltag saison 1))) x
        (reduce (fn [a i] (new-rating-shots a
                                           (sp/spieltag-shots saison i)))
                x (range 1 spieltag-nr))))

