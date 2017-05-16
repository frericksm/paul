(ns paulkrake.predict
  (:require [paulkrake.glicko2 :as g]
            [paulkrake.score :as s]
            [paulkrake.statistik :as st]))

(defn- predict [data heim gast faktor-sigma]
  (let [h-data  (get-in  data [heim] s/aufsteiger-rating-data)
        g-data  (get-in  data [gast] s/aufsteiger-rating-data)
        h-angriff-r  (get-in h-data [:angriff :rating])
        h-angriff-rd (get-in h-data [:angriff :rating-deviation])
        h-abwehr-r  (get-in h-data [:abwehr :rating])
        h-abwehr-rd (get-in h-data [:abwehr :rating-deviation])
        g-abwehr-r  (get-in g-data [:abwehr :rating])
        g-abwehr-rd (get-in g-data [:abwehr :rating-deviation])
        g-angriff-r  (get-in g-data [:angriff :rating])
        g-angriff-rd (get-in g-data [:angriff :rating-deviation])
        h-score-min (g/E-fn (g/mu (- h-angriff-r
                                     (* faktor-sigma h-angriff-rd)))
                            (g/mu g-abwehr-r)
                            (g/phi g-abwehr-rd))
        h-score-max (g/E-fn (g/mu (+ h-angriff-r
                                     (* faktor-sigma h-angriff-rd)))
                            (g/mu g-abwehr-r)
                            (g/phi g-abwehr-rd))
        g-score-max (- 1.0  (g/E-fn (g/mu (- h-abwehr-r
                                             (* faktor-sigma h-abwehr-rd)))
                                    (g/mu g-angriff-r)
                                    (g/phi g-angriff-rd )))
        g-score-min (- 1.0  (g/E-fn (g/mu (+ h-abwehr-r
                                             (* faktor-sigma h-abwehr-rd)))
                                    (g/mu g-angriff-r)
                                    (g/phi g-angriff-rd )))]
    [[h-score-min h-score-max]
     [g-score-min g-score-max]]))

(defn predict-single-game [data h g faktor-sigma]
  (as-> (predict data h g faktor-sigma) x
        (concat [h g] x)))

(defn predict-score [data games]
  (as-> games x
        (map (fn [[ h g]]
               (predict-single-game data h g 0.0)) x)
        (map (fn [[h g [hmin hmax] [gmin gmax]]]
               [h g hmin gmin]) x)))
