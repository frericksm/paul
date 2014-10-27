(ns paulkrake.shots
  (:require [paulkrake.score :as s]
            [paulkrake.predict :as p]))

(defn shots-to-score-fn
  "Maps number-of-goals to a score value from [0 1]"
  [number-of-goals]
  (/ (min 24 number-of-goals) 25.0))

(defn score-to-shots-fn [score]
  (as-> (range) x
        (partition 2 1 x)
        (filter (fn [[a b]]
                  (and (<= (shots-to-score-fn a) score )
                       (< score (shots-to-score-fn b)))) x)
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
