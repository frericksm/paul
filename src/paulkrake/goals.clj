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

(defn goals-data
  ([spieltage g-to-s-fn]
     (let [vereine (as-> spieltage x
                         (map first x)
                         (set x)
                         (map (fn [s]
                                (s/vereine (dc/spieltag s 1))) x)
                         (apply concat x)
                         (set x))]
       (reduce (fn [a [saison i]]
                 (s/new-rating a
                               (dc/spieltag saison i)
                               g-to-s-fn))
               (s/initial-rating-data vereine)
               spieltage))))

(defn fire [v] (if (> v 0.5) 1 0))

(defn calc [saison t n]
  (let [new_saison_1 (as-> saison x
                           (str x)
                           (.substring x 0 2)
                           (Integer/valueOf x))
        new_saison_2 (- new_saison_1 (int (/ (+ n (- 34 t)) 34)))
        new_saison_3 (format "%02d%02d" (mod new_saison_2 100) (mod (inc new_saison_2) 100))
        new_tag_1  (mod (- t n) 34) 
        new_tag_2  (if (= 0 new_tag_1) 34 new_tag_1)]
    [new_saison_3 new_tag_2]))

(defn range-spieltage [saison spieltag n]
  (as-> (range 1 (inc n)) x
        (map (fn [i] (calc saison spieltag i)) x)
        (reverse x)
        ))

(defn predict-result
  [saison spieltag n]
  (let [games (dc/spieltag saison spieltag)
        spieltage (range-spieltage saison spieltag n)]
    (as-> (range 0 10) x
          (reduce (fn [a i]
                    (let [d1 (goals-data spieltage (goals-to-score-fn-factory i))
                          new_score (p/predict-score d1 games)]
                      (assoc a i new_score)))
                  {} x)
          (map (fn [[i data]]
                 [i (map (fn [[h g hp gp]] [h g (* i (fire hp)) (* i(fire gp))]) data)])
               x)
          (map (fn [[i data]] (reduce (fn [a [h g hg gg]] 
                                       (assoc a [h g] [hg gg])) {} data)) x)
          (apply merge-with (fn [[h1 g1] [h2 g2]] [(max h1 h2) (max g1 g2)]) x)
          (map (fn [[h g hg gg]] [h g (first (get x [h g])) (second (get x [h g]))]) games)
          )))
