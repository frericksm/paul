(ns paulkrake.goals
  (:require [paulkrake.score :as s]
            [paulkrake.predict :as p]
            [paulkrake.spielplan :as sp]
            [paulkrake.shots :as shots]
            [paulkrake.goals-per-shots :as gps]
            [paulkrake.shots :as shots]
            [incanter.distributions :as d]))

(defn goals-to-score-fn
  "Maps number-of-goals to a score value from [0 1]. The mean of goals per game is 1.36"
  [goals]
  (let [goals-as-number (if (number? goals) goals (Integer/valueOf goals))]
    (d/cdf (d/poisson-distribution (* 2  1.36) ) (* 2 goals-as-number)) ;; * 2 to smooth the poisson distribution
    ))

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

(defn goals-data [saison spieltag-nr]
  (as-> (s/initial-rating-data (s/vereine (sp/spieltag saison 1))) x
        (reduce (fn [a i] (new-rating-goals a
                                           (sp/spieltag saison i)))
                x (range 1 spieltag-nr))))

(defn predict-goals-on-data
  ([data games]
     (predict-goals-on-data data games 0.0))
  ([data games faktor-sigma]
     (as-> games x
           (map (fn [[ h g]] (p/predict-single-game data h g faktor-sigma score-to-goals-fn)) x)
           (map (fn [[h g [hmin hmax] [gmin gmax]]] (format "%24s - %24s   [%.2f - %.2f] : [%.2f - %.2f]" h g hmin hmax gmin gmax)) x))))

(defn predict-goals
  ([saison spieltag-nr]
     (predict-goals saison spieltag-nr 0.0))
  ([saison spieltag-nr faktor-sigma]
     (predict-goals-on-data (goals-data saison spieltag-nr) (sp/spieltag saison spieltag-nr) faktor-sigma)))

(defn predict
  ([saison spieltag-nr]
     (predict saison spieltag-nr 0.0))
  ([saison spieltag-nr faktor-sigma]
     (let [shots-data (shots/shots-data saison spieltag-nr)
           gps-data   (gps/gps-data saison spieltag-nr)
           games      (sp/spieltag saison spieltag-nr)]
       (as-> games x
             (map (fn [[ h g]]
                    (let [[_ _ [shots-hmin shots-hmax] [shots-gmin shots-gmax]] (p/predict-single-game shots-data h g faktor-sigma shots/score-to-shots-fn)
                          [_ _ [gps-hmin gps-hmax] [gps-gmin gps-gmax]] (p/predict-single-game gps-data h g faktor-sigma gps/score-to-gps-fn)]
                      [h g [(* shots-hmin gps-hmin 1) (* shots-hmax gps-hmax 1)] [(* shots-gmin gps-gmin 1) (* shots-gmax gps-gmax 1)] ])) x)
             (map (fn [[h g [hmin hmax] [gmin gmax]]] (format "%24s - %24s   [%.2f - %.2f] : [%.2f - %.2f]" h g hmin hmax gmin gmax)) x))
       )))
