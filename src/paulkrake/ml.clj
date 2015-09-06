(ns paulkrake.ml
  (:require [paulkrake.datacenter :as dc]))

(defn fmt [v]
  (/ (int (* 100 (bigdec v))) 100.0))

(defn create-features [saison spieltag n out-file]
  (as-> (dc/range-spieltage saison spieltag n) z
    (map (fn [[s t]] (dc/stat-data s t)) z)
    (apply concat z)
    (map (fn [[_ _ s1 s2]]
           {:x (concat
                (map (fn [k] (fmt (get s1 k))) dc/kategories)
                (map (fn [k] (fmt (get s2 k))) dc/kategories))
            :y (vector (get s1 "score") (get s2 "score"))}) z)
    (map (fn [{:keys [x y]}] 
           (format "%s,%s" 
                   (as-> x a
                     (interpose "," a)
                     (apply str a))
                   (as-> y a
                     (interpose "," a)
                     (apply str a)))) z)
    (interpose (System/getProperty "line.separator") z)
    (apply str z)
    (spit out-file z)))
