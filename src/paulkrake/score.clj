(ns paulkrake.score
  (:require [paulkrake.glicko2 :as g]))

;; Base zur Berechnung des Scores aus den Goals
(def base 0.46)

(def start-rating-data {:abwehr {:rating 1500.0
                                 :rating-deviation 350.0
                                 :volatility 0.06}
                        :angriff {:rating 1500.0
                                  :rating-deviation 350.0
                                  :volatility 0.06}})

(def aufsteiger-rating-data {:abwehr {:rating 1400.0
                                      :rating-deviation 350.0
                                      :volatility 0.06}
                             :angriff {:rating 1400.0
                                       :rating-deviation 350.0
                                       :volatility 0.06}})

(defn initial-rating-data [vereine]
  (reduce (fn [a v] (assoc a v start-rating-data)) {} vereine))

(defn get-rating-data [data verein abwehr-or-sturm-keyword]
  (get-in data [verein abwehr-or-sturm-keyword]))

(defn update-rating-data [data verein abwehr-or-sturm-keyword rating-data]
  (assoc-in data [verein abwehr-or-sturm-keyword] rating-data))

(defn score-fn [number-of-goals]
  (let [goals-as-number (if (number? number-of-goals)
            number-of-goals
            (Integer/valueOf number-of-goals))]
    (- 1.0 (Math/pow base goals-as-number))))

(defn abwehr-score-fn [number-of-goals]
  (- 1.0 (score-fn number-of-goals)))

(defn goal-fn [score]
  (as-> (range) x
        (partition 2 1 x)
        (filter (fn [[a b]]
                  (and (<= (score-fn a) score )
                       (< score (score-fn b)))) x)
        (map (fn [[a b]]
               (+ a (*  (/ (- score (score-fn a)) 
                           (- (score-fn b) (score-fn a))) 
                        (- b a)))) x)
        (first x)))

(defn games-of [games verein]
  (as-> games x
        (filter (fn [[h g hg gg]] (and (not (nil? hg)) (not (nil? gg)))) x)
        (filter (fn [[h g]] (contains? #{h g} verein)) x)))

(defn gegner [verein games]
  (as-> games x
        (games-of x verein)
        (map (fn [[heim gast heim-goals gast-goals]] (if (= verein heim) gast heim)) x)))

(defn scores [verein games]
  (as-> games x
        (games-of x verein)
        (map (fn [[heim gast heim-goals gast-goals]]
               (let [abwehr-score (if (= verein heim)
                                    (abwehr-score-fn gast-goals)
                                    (abwehr-score-fn heim-goals))
                     angriff-score (if (= verein heim)
                                     (score-fn heim-goals)
                                     (score-fn gast-goals))]
                 {:abwehr abwehr-score :angriff angriff-score})) x)))

(defn new-vereins-rating [data verein games]
  (let [games-of-verein (games-of games verein)
        verein-rating (get data verein)
        gegner (gegner verein games-of-verein)
        scores (scores verein games-of-verein)
        scores-abwehr (map :abwehr scores)
        scores-angriff (map :angriff scores)
        old-rating-abwehr (get-in verein-rating [:abwehr :rating] )
        old-rating-deviation-abwehr (get-in verein-rating [:abwehr :rating-deviation])
        old-volatility (get-in verein-rating [:abwehr :volatility])
        oppenents-rating-angriff (as-> gegner x
                                       (map #(get data %) x)
                                       (map #(get-in % [:angriff :rating]) x))
        oppenents-rating-deviation-angriff (as->
                                            gegner x
                                            (map #(get data %) x)
                                            (map #(get-in % [:angriff :rating-deviation]) x))
        old-rating-angriff (get-in verein-rating [:angriff :rating] )
        old-rating-deviation-angriff (get-in verein-rating
                                             [:angriff :rating-deviation])
        oppenents-rating-abwehr (as-> gegner x
                                      (map #(get data %) x)
                                      (map #(get-in % [:abwehr :rating]) x))
        oppenents-rating-deviation-abwehr (as-> gegner x
                                                (map #(get data %) x)
                                                (map #(get-in % [:abwehr :rating-deviation]) x))
        new-abwehr-rating-data (g/adjust-rating old-rating-abwehr
                                                old-rating-deviation-abwehr
                                                old-volatility
                                                oppenents-rating-angriff
                                                oppenents-rating-deviation-angriff
                                                scores-abwehr)
        new-angriff-rating-data (g/adjust-rating old-rating-angriff
                                                 old-rating-deviation-angriff
                                                 old-volatility
                                                 oppenents-rating-abwehr
                                                 oppenents-rating-deviation-abwehr
                                                 scores-angriff)
        ]
    {:abwehr new-abwehr-rating-data :angriff new-angriff-rating-data}
    ))

(defn vereine [games]
  (->> games
       (map (fn [[h g _]] [h g]))
       flatten
       set))

(defn played-games [games]
  (filter (fn [[h g hg gg ]] (and (not (nil? hg) ) (not (nil? gg)))) games))

(defn new-rating [data games]
  (as-> (vereine games) x        
        (map (fn [v] [v (new-vereins-rating data v games)]) x)
        (into {} x)))

(defn tabelle
  ([data]
     (->> data
          (sort-by (fn [[name m]]
                     (let [ang-r (Math/pow  (get-in m [:angriff :rating]) 2)
                           abw-r (Math/pow  (get-in m [:abwehr :rating])  2)]
                       (* -1.0 (Math/sqrt (+ ang-r abw-r) )))))
          (map (fn [[name m]] 
                 (format "%-25s : %.2f % 7.2f   %.2f % 7.2f" name  
                         (get-in m [:angriff :rating]) 
                         (get-in m [:angriff :rating-deviation])
                         (get-in m [:abwehr :rating])
                         (get-in m [:abwehr :rating-deviation]))))
          (cons (format "%-25s : %7s %7s   %7s %7s" "Verein" "Ang" "std" "Abw" "std"))
          ))
  ([data angriff-abwehr-keyword]
     (->> data
          (sort-by (fn [[name m]] 
                     (* -1.0 (get-in m [angriff-abwehr-keyword :rating]))))
          (map (fn [[name m]] 
                 (format "%-30s : %5.2f %5.2f" name  
                         (get-in m [:angriff :rating]) 
                         (get-in m [:abwehr :rating]))))
           )))
