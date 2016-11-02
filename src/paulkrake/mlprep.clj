(ns paulkrake.mlprep
  (:require [paulkrake.datacenter :as dc]
            [paulkrake.goals :as g]))

(defn alle-vereine []
  (as-> (range 31) x
          (into []
                (comp 
                 (map (fn [i] (dc/spieltag-add 1516 1 (* -34 i))))
                 (map (fn [[s t]] (dc/spieltag s t)))
                 (map (fn [s] (map (fn [[h g _ _]] (vector h g)) s)))
                 (map flatten))
                x)
          (apply concat x)
          (set x)
          (sort x)))

(defn spieltag-to-map [s t] 
  (as-> (dc/spieltag s t) x
    (reduce (fn [a [h g hg gg]]
              (assoc a [h g] [hg gg])) {} x)))

(defn input-vector [[s t :as spieltag] alle-vereine]
  (let [flag-fn (fn [b] (if b 1 0))
        
        this-spieltag (spieltag-to-map s t )
        [ns nt] (dc/spieltag-after s t)
        next-spieltag (spieltag-to-map  ns nt)
        m1 (for [h alle-vereine g alle-vereine :when (not= h g)]
             (let [[hg gg] (get this-spieltag [h g] [0 0])]
               (vector (flag-fn (contains? this-spieltag [h g]))
                       hg gg
                       (flag-fn (contains? next-spieltag [h g])))))]
    (as-> m1 x
      (apply concat x))))

(defn output-vector [[s t :as spieltag]]
  (let [[ns nt] (dc/spieltag-after s t)
        next-spieltag (sort-by first (dc/spieltag  ns nt))
        m1 (map (fn [[h g hg gg]] [hg gg]) next-spieltag)]
    (as-> m1 x
      (apply concat x))))


#_(as-> (dc/spieltag 1516 1) x
        (reduce (fn [a [h g hg gg]] (assoc a (vector h g) (vector hg gg))) {} x)
        (for [h alle-vereine g alle-vereine]
          (vector [h g] (contains? x [h g]))))


#_(as-> (range 1 44) x
        (map (fn [i] (spieltag-add 1617 10 (* -1 i))) x )
        (map (fn [[s t]] (measure  (spieltag s t) ((predict-fn-factory 29 median metric-kickerpoints) s t) metric-kickerpoints)) x)
        (/ (apply + x) 43.0)
       )
