(ns paulkrake.crossval
  (:require [paulkrake.datacenter :as dc]
            [paulkrake.goals :as g]))


(defn cost-single-game [[[rh rg][ph pg]]]
  (let []
    (cond (and (=  rh ph) (= rg pg)) 4
          (and (not= rh ph) (= (- rh ph) (- rg pg)))    3
          (= (Math/signum (double (- rh rg))) (Math/signum (double (- ph pg)))) 2
          true                                                0
          )))
(defn look-back [min-n max-n  t]
  (max min-n (min t max-n)))

(defn cost [[s t] min-n max-n]
  (let [n (look-back min-n max-n t)
        result (as-> (dc/spieltag s t) x 
                 (reduce (fn [a [h g hg gg]] (assoc a [h g] [(int hg) (int gg) ])) nil x))
        prediction (as-> (g/predict-result s t n) x 
                     (reduce (fn [a [h g hg gg]] (assoc a [h g] [(int hg) (int gg)])) nil x))]
    (as-> (merge-with vector result prediction) x
      (vals x)
      (map (fn [rp] (cost-single-game rp)) x)
      (apply + x))))

;;
(as-> (range-spieltage 1213 34 34) x
  (map (fn [s] (cost s 10 27)) x)
  (apply + x)
  )
