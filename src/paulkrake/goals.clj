(ns paulkrake.goals
  (:require [paulkrake.score :as s]
            [paulkrake.predict :as p]
            [paulkrake.spielplan :as sp]
            [paulkrake.datacenter :as dc]
            [paulkrake.shots :as shots]
            [paulkrake.goals-per-shots :as gps]
            [paulkrake.shots :as shots]
           ))

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

(defn goals-data
  [spieltage g-to-s-fn]
  ;;(println "goals-data start" spieltage)
  (let [vereine (as-> spieltage x
                  (map first x)
                  (set x)
                  (map (fn [s] (s/vereine (dc/spieltag s 1))) x)
                  (apply concat x)
                  (set x))
        result (reduce (fn [a [saison i]] (s/new-rating a (dc/spieltag saison i) g-to-s-fn))
                       (s/initial-rating-data vereine)
                       spieltage)]
    result))

(defn fire [v] (if (> v 0.5) 1 0))

(defn predict-result-unmemoized
  [saison spieltag n]
  (let [games (dc/spieltag saison spieltag)
        spieltage (dc/range-spieltage saison spieltag n)]
    (as-> (range 0 10) x
      (reduce (fn [a i] (as-> i y
                          (goals-to-score-fn-factory y)
                          (goals-data spieltage y)
                          (p/predict-score y games)
                          (assoc a i y)))
              {} x)
      (map (fn [[i data]]
             [i (map (fn [[h g hp gp]] 
                       [h g (* i (fire hp)) (* i (fire gp))]) 
                     data)])
           x)
      (map (fn [[i data]] (reduce (fn [a [h g hg gg]] 
                                    (assoc a [h g] [hg gg])) {} data)) x)
      (apply merge-with (fn [[h1 g1] [h2 g2]] 
                          [(max h1 h2) (max g1 g2)]) x)
      (map (fn [[h g hg gg]] 
             [h g (first (get x [h g])) (second (get x [h g]))]) 
           games))))

(def predict-result (memoize predict-result-unmemoized))

(defn ratings
  [saison spieltag n]
  (let [games (dc/spieltag saison spieltag)
        spieltage (dc/range-spieltage saison spieltag n)]
    (as-> (range 0 10) x
      (reduce (fn [a i] (as-> i y
                          (goals-to-score-fn-factory y)
                          (goals-data spieltage y)
                          (assoc a i y)))
              {} x))))


