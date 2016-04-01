(ns paulkrake.crossval
  (:require [paulkrake.datacenter :as dc]
            [paulkrake.goals :as g]))

(defn metric-distance 
  "euklidische Norm"
  [result prediction]
  (as-> (map (fn [[rh rg][ph pg]]
               (Math/sqrt (+ (* (- rg pg) 
                                (- rg pg)) 
                             (* (- rh ph) 
                                (- rh ph))))) result prediction) x
    (map (fn [x] (* x x)) x)
    (apply + x)
    (Math/sqrt x)
    (* 100 x)
    (int x)
    (/ x 100.0)))

(defn kickerpoints [[rh rg][ph pg]]
  (cond (and (=  rh ph) (= rg pg)) 4
        (and (not= rh ph) (= rh rg) (= ph pg))    2
        (and (not= rh ph) (= (- rh ph) (- rg pg)))    3
        (= (Math/signum (double (- rh rg))) (Math/signum (double (- ph pg)))) 2
        true                                                0))

(defn metric-kickerpoints 
  "kickerpoints metric"
  [result prediction]
  (->> (map kickerpoints result prediction)
       (map (fn [x] (* -1 x)))
       (apply +)))

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
      (map (fn [rp] (metric-kickerpoints rp)) x)
      (apply + x))))


#_(defn cross-validate [saison threshold]
  (as-> (for [max-n (range 1 35)  min-n (range 1 35) :while (<= min-n max-n)]
          (as-> (dc/range-spieltage saison 35 34) y
            (map (fn [s] (points s min-n max-n)) y)
            [min-n max-n (apply + y)])) x
    (filter (fn [[_ _ p]] (< threshold p)) x )
    (sort-by (fn [[a b _]] (+ b  (* a  100)) ) x)))
#_(cross-validate 1415 386)

(defn to-map [list-of-results]
  (reduce (fn [a [h g hg gg]] (assoc a [h g] [hg gg])) {} list-of-results))

(defn measure [result prediction metric-fn]
  (as-> (merge-with vector (to-map result) (to-map prediction)) x
    (vals x)
    (metric-fn (map first x) (map second x))))

(defn measure-prediction [s t n metric-fn]
  (let [result (dc/spieltag s t)
        prediction (g/predict-result s t n)] 
    (measure result prediction metric-fn)))

(defn lookback [s t metric-fn]
  (let [result (dc/spieltag s t)]
    (as-> (range 5 34) x1
      (map (fn [n] [n (measure-prediction s t n metric-fn)]) x1)
      (sort-by first x1))))


(defn optimal-lookback 
  "Ruft für die letzen n Spieltage vor [s t] die Funktion looback auf.
   Die Ergebnisse der Aufrufe werden in eine Map umgewandelt und mit der Funktion + gemergt.
   Die Funktion gibt die summierten lookback Ergebnisse mit den höchsten Punktzahlen zurück.
 
   s : Saison, z.B 1516 
   t : der nächste ungespielte Spieltag, z.B 28
   n : über die letzten n Spieltage soll summiert werden
   metric-fn : die Metrik-Funktion mit der die Abweichung von Ergebnis und Vorhersage berechnet werden soll 

 
"
  [s t n metric-fn]
  (as->  (dc/range-spieltage s t n) x
        (map (fn [[s t]] (lookback s t metric-fn)) x)
        (map (fn [lb] (into {} lb)) x)
        (apply merge-with + x)
        (sort-by second x)  
        #_(reverse x)
        (take 3 x)))


