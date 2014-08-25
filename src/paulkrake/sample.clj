(ns paulkrake.sample
  (:require [paulkrake.glicko2 :as g]))

;; step 1

;; step 2
(comment 

  (def ratings [1400.0 1550.0 1700.0])

  (def scores  [1.0 0.0 0.0])

  (def rating-deviations [30.0 100.0 300.0])

  (def mu-val (mu 1500))

  (def phi-val (phi 200))

  (def sigma 0.06)

  (def mus (->> ratings (map mu)))

  (def phis (->> rating-deviations (map phi)))

  (def v (v-fn mu-val
               (->> ratings (map mu)) 
               (->> rating-deviations (map phi))))

  (def delta-val (delta-fn v mu-val mus phis scores))

  (def sig-strich  (sigma-strich sigma phi-val delta-val v))

  (def phi-star-val (phi-star phi-val sig-strich ))


  (def new-phi-val (new-phi v phi-star-val))

  (def new-mu-val (new-mu mu-val new-phi-val mus phis scores)))
