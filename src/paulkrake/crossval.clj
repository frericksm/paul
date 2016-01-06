(ns paulkrake.crossval
  (:require [paulkrake.datacenter :as dc]
            [paulkrake.goals :as g]))


(defn points-single-game [[[rh rg][ph pg]]]
  (let []
    (cond (and (=  rh ph) (= rg pg)) 4
          (and (not= rh ph) (= (- rh ph) (- rg pg)))    3
          (= (Math/signum (double (- rh rg))) (Math/signum (double (- ph pg)))) 2
          true                                                0
          )))
(defn look-back [min-n max-n  t]
  (max min-n (min t max-n)))

(defn points [[s t] min-n max-n]
  (let [n (look-back min-n max-n t)
        result (as-> (dc/spieltag s t) x 
                 (reduce (fn [a [h g hg gg]] (assoc a [h g] [(int hg) (int gg) ])) nil x))
        prediction (as-> (g/predict-result s t n) x 
                     (reduce (fn [a [h g hg gg]] (assoc a [h g] [(int hg) (int gg)])) nil x))]
    (as-> (merge-with vector result prediction) x
      (vals x)
      (map (fn [rp] (points-single-game rp)) x)
      (apply + x))))


(defn cross-validate [saison threshold]
  (as-> (for [min-n (range 1 20) max-n (range 1 20)]
          (as-> (dc/range-spieltage saison 35 34) y
            (map (fn [s] (points s min-n max-n)) y)
            [min-n max-n (apply + y)])) x
    (filter (fn [[_ _ p]] (< threshold p)) x )
    (sort-by (fn [[a b _]] (+ b  (* a  100)) ) x)))


#_(cross-validate 1415 386)
