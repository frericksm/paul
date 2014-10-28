(ns paulkrake.goals-per-shots
  (:require [paulkrake.score :as s]
            [paulkrake.predict :as p]
            [paulkrake.spielplan :as sp]
            [incanter.distributions :as d]))

(defn gps-to-score-fn
  "Maps number of gps to a score value from [0 1].
   The mean of gps per game is 0.1177"
  [gps]
  (d/cdf (d/poisson-distribution 11.77) gps))

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

(defn predict-gps
  ([data games]
     (predict-gps data games 1.0))
  ([data games faktor-sigma]
     (as-> games x
           (map (fn [[ h g]] (p/predict-single-game data h g faktor-sigma score-to-gps-fn)) x)
           (map (fn [[h g [hmin hmax] [gmin gmax]]] (format "%24s - %24s   [%.2f - %.2f] : [%.2f - %.2f]" h g hmin hmax gmin gmax)) x))))

(defn gps-data [saison spieltag-nr]
  (as-> (s/initial-rating-data (s/vereine (sp/spieltag saison 1))) x
        (reduce (fn [a i] (new-rating-gps a
                                         (sp/spieltag-treffer-pro-shots 1415 i)))
                x (range 1 spieltag-nr))))

