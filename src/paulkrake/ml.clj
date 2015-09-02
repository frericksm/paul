(ns paulkrake.ml
  (:require [paulkrake.datacenter :as dc]))

(defn fmt [v]
  (/ (int (* 100 (bigdec v))) 100.0))

(defn score-to-label 
  "Takes a soccer result and translates it into a unique number"
  [g1 g2]
  (let [n (+ g1 g2)
        s (/ (* n (inc n)) 2)]
    (+ s (inc g2))))

(defn label-to-score 
  "Takes a label and translates it back into a soccer result"
  [label]
  (let [g1_0 (as-> (range 0 1000) x
               (filter (fn [i] (<= (score-to-label i 0) label)) x)
               (last x))
        n (/ (* g1_0 (inc g1_0)) 2)
        g2 (dec (- label n))
        g1  (- g1_0 g2)]
    [g1 g2]))

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
