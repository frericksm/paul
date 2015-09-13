(ns paulkrake.score
  (:require [paulkrake.datacenter :as dc]
            [paulkrake.glicko2 :as g]
            [incanter.distributions :as d]))

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

(defn games-of [games verein]
  (as-> games x
        (filter (fn [[h g hg gg]] (and (not (nil? hg)) (not (nil? gg)))) x)
        (filter (fn [[h g]] (contains? #{h g} verein)) x)))

(defn gegner [verein games]
  (as-> games x
        (games-of x verein)
        (map (fn [[heim gast heim-goals gast-goals]] (if (= verein heim) gast heim)) x)))

(defn scores [verein games score-fn]
    (let [abwehr-score-fn (fn [v] (- 1.0 (score-fn v)))]
    (as-> games x
          (games-of x verein)
          (map (fn [[heim gast heim-goals gast-goals]]
                 (let [abwehr-score (if (= verein heim)
                                      (abwehr-score-fn gast-goals)
                                      (abwehr-score-fn heim-goals))
                       angriff-score (if (= verein heim)
                                       (score-fn heim-goals)
                                       (score-fn gast-goals))]
                   {:abwehr abwehr-score :angriff angriff-score})) x))))

(defn new-vereins-rating [data verein games score-fn]
  (let [games-of-verein (games-of games verein)
        verein-rating (get data verein)
        gegner (gegner verein games-of-verein)
        scores (scores verein games-of-verein score-fn)
        scores-abwehr (map :abwehr scores)
        scores-angriff (map :angriff scores)
        old-rating-abwehr (get-in verein-rating [:abwehr :rating] )
        old-rating-deviation-abwehr (get-in verein-rating [:abwehr :rating-deviation])
        old-volatility-abwehr (get-in verein-rating [:abwehr :volatility])
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
        old-volatility-angriff (get-in verein-rating [:angriff :volatility])
        oppenents-rating-abwehr (as-> gegner x
                                      (map #(get data %) x)
                                      (map #(get-in % [:abwehr :rating]) x))
        oppenents-rating-deviation-abwehr (as-> gegner x
                                                (map #(get data %) x)
                                                (map #(get-in % [:abwehr :rating-deviation]) x))]
    {:abwehr (g/adjust-rating old-rating-abwehr
                              old-rating-deviation-abwehr
                              old-volatility-abwehr
                              oppenents-rating-angriff
                              oppenents-rating-deviation-angriff
                              scores-abwehr)
     :angriff (g/adjust-rating old-rating-angriff
                               old-rating-deviation-angriff
                               old-volatility-angriff
                               oppenents-rating-abwehr
                               oppenents-rating-deviation-abwehr
                               scores-angriff)}))

(defn vereine [games]
  (->> games
       (map (fn [[h g _]] [h g]))
       flatten
       set))

(defn played-games [games]
  (filter (fn [[h g hg gg ]] (and (not (nil? hg) ) (not (nil? gg)))) games))

(defn add-data-for-missing-teams [data teams]
  (reduce (fn [a t] (if (contains? a t) a (assoc a t start-rating-data))) data teams))

(defn new-rating [data games score-fn]
  ;;(println "new-rating start" )
  (let [teams (vereine games)
        d2 (add-data-for-missing-teams data teams)
        result (as-> teams x        
                 (map (fn [v] [v (new-vereins-rating d2 v games score-fn)]) x)
                 (into {} x))]
    result))

(defn apply-to-predicted-scores [inverse-fn [[hmin hmax] [gmin gmax]]]
  [[(inverse-fn hmin)  (inverse-fn hmax)] [(inverse-fn gmin) (inverse-fn gmax)]])

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
          (cons (format "%-25s : %7s %7s   %7s %7s" "Verein" "Ang" "std" "Abw" "std"))))
  
  ([data angriff-abwehr-keyword]
     (->> data
          (sort-by (fn [[name m]] 
                     (* -1.0 (get-in m [angriff-abwehr-keyword :rating]))))
          (map (fn [[name m]] 
                 (format "%-30s : %5.2f %5.2f" name  
                         (get-in m [:angriff :rating]) 
                         (get-in m [:abwehr :rating])))))))

(defn find-mean [saison spieltag-nr spieltag-fn]
  (as-> (range 1 spieltag-nr) x
        (map #(spieltag-fn saison %) x)
        (apply concat x)
        (map (fn [[_ _ h g]] [h g]) x)
        (apply concat x)
        (d/mean x)
        (double x)))

(defn kategorie-data
  ([spieltage kategorie kategorie-to-score-fn]
     (let [vereine (as-> spieltage x
                         (map first x)
                         (set x)
                         (map (fn [s]
                                (vereine (dc/data s 1 kategorie))) x)
                         (apply concat x)
                         (set x))]
       (reduce (fn [a [saison i]]
                 (new-rating a
                               (dc/data saison i kategorie)
                               kategorie-to-score-fn))
               (initial-rating-data vereine)
               spieltage))))
