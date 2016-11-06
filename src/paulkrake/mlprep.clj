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

(defn flag-fn [b] (if b 1 0))

(defn spieltag-as-list [[s t :as spieltag] alle-vereine]
  (let [sp (reduce (fn [a [h g hg gg]]
                     (as-> a x
                       (assoc x h hg)
                       (assoc x g gg))) {} (dc/spieltag s t))]
    (as-> alle-vereine x
      (map (fn [v] (get sp v -1)) x))))

(defn spieltag-heim-gast-as-list [[s t :as spieltag] alle-vereine]
  (let [sp (reduce (fn [a [h g hg gg]]
                     (as-> a x
                       (assoc x h 1)
                       (assoc x g -1))) {} (dc/spieltag s t))]
    (as-> alle-vereine x
      (map (fn [v] (get sp v 0)) x))))

(defn input-vector [[s t :as spieltag] alle-vereine]
  (let [this-heim-gast-liste (spieltag-heim-gast-as-list spieltag alle-vereine)
        spieltag (spieltag-as-list spieltag alle-vereine)
        next-heim-gast-liste (spieltag-heim-gast-as-list (dc/spieltag-after s t)
                                                         alle-vereine)]
    (concat this-heim-gast-liste spieltag next-heim-gast-liste)))

(defn output-vector [[s t :as spieltag] alle-vereine]
  (spieltag-as-list (dc/spieltag-after s t) alle-vereine))


#_(as-> (dc/spieltag 1516 1) x
        (reduce (fn [a [h g hg gg]] (assoc a (vector h g) (vector hg gg))) {} x)
        (for [h alle-vereine g alle-vereine]
          (vector [h g] (contains? x [h g]))))


#_(as-> (range 1 44) x
        (map (fn [i] (spieltag-add 1617 10 (* -1 i))) x )
        (map (fn [[s t]] (measure  (spieltag s t) ((predict-fn-factory 29 median metric-kickerpoints) s t) metric-kickerpoints)) x)
        (/ (apply + x) 43.0)
       )
