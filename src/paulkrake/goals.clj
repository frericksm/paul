(ns paulkrake.goals
  (:require [paulkrake.score :as s]
            [paulkrake.predict :as p]))

(defn goals-to-score-fn
  "Maps number-of-goals to a score value from [0 1]"
  [number-of-goals]
  (let [base 0.46   ;; Base zur Berechnung des Scores aus den Goals
        goals-as-number (if (number? number-of-goals)
                          number-of-goals
                          (Integer/valueOf number-of-goals))]
    (- 1.0 (Math/pow base goals-as-number))))

(defn score-to-goals-fn [score]
  (as-> (range) x
        (partition 2 1 x)
        (filter (fn [[a b]]
                  (and (<= (goals-to-score-fn a) score )
                       (< score (goals-to-score-fn b)))) x)
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
