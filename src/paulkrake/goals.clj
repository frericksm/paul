(ns paulkrake.goals
  (:require [paulkrake.score :as s]
            [paulkrake.predict :as p]
            [paulkrake.spielplan :as sp]
            [paulkrake.datacenter :as dc]
            [paulkrake.shots :as shots]
            [paulkrake.goals-per-shots :as gps]
            [paulkrake.shots :as shots]
            [incanter.distributions :as d]))

(defn goals-to-score-fn-factory 
  "Returns a function of one parameter g mapping int -> #{0 1}. Returns 0 if g < min-goals. Returns 1 if g >= min-goals."
  [min-goals]
  (fn [goals]
    (let [goals-as-number (if (number? goals) 
                            goals 
                            (Integer/valueOf goals))
          min-goals-as-number (if (number? min-goals) 
                                min-goals 
                                (Integer/valueOf min-goals))]
      (if (>= goals-as-number min-goals-as-number) 1 0))))

(defn goals-to-score-fn
  "Maps number-of-goals to a score value from [0 1]. The mean of goals per game is 1.36"
  [goals]
  (let [goals-as-number (if (number? goals) goals (Integer/valueOf goals))
        scale 1  ;; to smooth the poisson distribution
        ]
    (d/cdf (d/poisson-distribution (* scale  1.36) ) (* scale goals-as-number)) 
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

(defn new-rating-goals 
  ([data games g-to-s-fn]
   (s/new-rating data games g-to-s-fn))
  ([data games]
   (new-rating-goals data games goals-to-score-fn)))

(defn goals-data
  ([saison spieltag-nr-von spieltag-nr-bis g-to-s-fn]
   (as-> (s/initial-rating-data (s/vereine (dc/spieltag saison 1))) x
     (reduce (fn [a i] (new-rating-goals a
                                         (dc/spieltag saison i)
                                         g-to-s-fn))
             x (range spieltag-nr-von spieltag-nr-bis))))
  ([saison spieltag-nr-von spieltag-nr-bis]
   (goals-data saison spieltag-nr-von spieltag-nr-bis goals-to-score-fn))
  ([saison spieltag-nr-bis]
   (goals-data saison 1 spieltag-nr-bis)))

(defn predict-goals-on-data
  ([data games]
     (predict-goals-on-data data games 0.0))
  ([data games faktor-sigma]
     (p/predict-on-data data games faktor-sigma score-to-goals-fn)))

(defn fire [v] (if (>= v 0.5) 1 0))

(defn predict-result [saison spieltag-von spieltag-bis games]
  (as-> (range 0 10) x
    (reduce (fn [a i] 
              (assoc a i 
                     (p/predict-score 
                      (goals-data saison spieltag-von spieltag-bis 
                                  (goals-to-score-fn-factory i))
                      games))) {} x)
    (map (fn [[i data]]
           [i (map (fn [[h g hp gp]] [h g (* i (fire hp)) (* i(fire gp))]) data)])
         x)
    (map (fn [[i data]] (reduce (fn [a [h g hg gg]] 
                                  (assoc a [h g] [hg gg])) {} data)) x)
    (apply merge-with (fn [[h1 g1] [h2 g2]] [(max h1 h2) (max g1 g2)]) x)))
