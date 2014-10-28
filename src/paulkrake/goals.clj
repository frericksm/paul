(ns paulkrake.goals
  (:require [paulkrake.score :as s]
            [paulkrake.predict :as p]
            [paulkrake.spielplan :as sp]
            [incanter.distributions :as d]))

(defn goals-to-score-fn
  "Maps number-of-goals to a score value from [0 1]. The mean of goals per game is 1.36"
  [goals]
  (let [goals-as-number (if (number? goals) goals (Integer/valueOf goals))]
    (d/cdf (d/poisson-distribution 1.36) goals-as-number)))

(defn score-to-goals-fn [score]
  (as-> (range -1 100) x
        (partition 2 1 x)
        (filter (fn [[a b]]
                  (and (<= (goals-to-score-fn a) score )
                       (<= score (goals-to-score-fn b)))) x)
        (map (fn [[a b]]
               (+ a (*  (/ (- score (goals-to-score-fn a)) 
                           (- (goals-to-score-fn b) (goals-to-score-fn a))) 
                        (- b a)))) x)
        (first x)))

(defn new-rating-goals [data games]
  (s/new-rating data games goals-to-score-fn))

(defn predict-goals
  ([data games]
     (predict-goals data games 1.0))
  ([data games faktor-sigma]
     (as-> games x
           (map (fn [[ h g]] (p/predict-single-game data h g faktor-sigma score-to-goals-fn)) x)
           (map (fn [[h g [hmin hmax] [gmin gmax]]] (format "%24s - %24s   [%.2f - %.2f] : [%.2f - %.2f]" h g hmin hmax gmin gmax)) x))))


(defn goals-data [saison spieltag-nr]
  (as-> (s/initial-rating-data (s/vereine (sp/spieltag 1415 1))) x
        (reduce (fn [a i] (new-rating-goals a
                                           (sp/spieltag 1415 i)))
                x (range 1 spieltag-nr))))
