(ns paulkrake.crossval
  (:require [paulkrake.datacenter :as dc]
            [paulkrake.goals :as g]
            [paulkrake.mwua :as m]))

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

(defn project-in-interval 
  "precondition: min-n <= max-n.
  Returns t if min < t < max, min if t < min and max if t > max"
  [min-n max-n t]
  (max min-n (min t max-n)))

(defn points [[s t] min-n max-n]
  (let [n (project-in-interval min-n max-n t)
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

(defn median [& seq]
  (let [c (count seq)
        v (vec (sort seq))]
    (if (even? c)
      (double (/ (+ (nth v (dec (int (/ c 2))))
                    (nth v (int (/ c 2)))) 2))
      (nth v (/ c 2)))))


(defn merge-to-vec [a b] 
  (cond (coll? a) (conj (vec a) b)
        (nil? a)  (vector b)
        (number? a) (vector a b)))

(defn optimal-lookback 
  "Ruft für die letzen n Spieltage vor [s t] die Funktion lookback auf.
   Die Ergebnisse der Aufrufe werden in eine Map umgewandelt und mit der Funktion metric-value-merge-fn (default: +) gemergt.
   Die Funktion gibt die summierten lookback Ergebnisse mit den höchsten Punktzahlen zurück.
 
   s : Saison, z.B 1516 
   t : Spieltag, z.B 28
   n : über die letzten n Spieltage soll summiert werden
   metric-fn : die Metrik-Funktion mit der die Abweichung von Ergebnis und Vorhersage berechnet werden soll
   metric-value-merge-fn : Funktion mit der die Ergebnisse des Aufrufs der metric-fn zusammengefasst wird. Z.b + oder median. Wird kein Wert für diesen Parameter übergeben, dann wird die Funktion + verwendet  
"
  ([s t n metric-fn metric-value-merge-fn]
   (as-> (dc/spieltag-after s t) x 
     (let [[s2 t2] x]
       (dc/range-spieltage s2 t2 n)) 
     (map (fn [[s t]] (lookback s t metric-fn)) x)
     (map (fn [lb] (into {} lb)) x)
     (reduce (fn [a m] (merge-with merge-to-vec a m)) {} x)
     (reduce-kv (fn [m k v] 
                  (let [v_seq (if (coll? v) v (vector v))]
                    (assoc m k (apply metric-value-merge-fn v_seq)))) {} x)
     (sort-by (fn [[n m]] (+ n (* 1000 m))) x)
     #_(ffirst x)))
  ([s t n metric-fn]
   (optimal-lookback s t n metric-fn +)
   ))


(defn predict-fn-factory [lookback metric-merge-fn metric-fn]
  (fn [s t]
    (let [[s2 t2] (dc/spieltag-add s t -1)
          olb (optimal-lookback s2 t2 lookback metric-fn metric-merge-fn)]
      (g/predict-result s t olb))))

(defn accurancy [sample-spieltage lookback metric-merge-fn metric-fn]
  (let [pf (predict-fn-factory lookback metric-merge-fn metric-fn)]
    (as-> sample-spieltage x
      (map (fn [st]
             (let [[s t] st
                   p (measure (dc/spieltag s t) (pf s t) metric-kickerpoints )]
               p)) x)
      (apply + x)
      (double (/ x (count sample-spieltage))))))


(comment 
  (def accurancy-evaluation
    (let [sample (dc/sample-of-spieltage 1415 34 900 200)]
      (for [l (range 1 10)
            mf [metric-kickerpoints metric-distance]
            mvf [+ median]]
        [[l mf mvf] (accurancy sample l mvf mf )])))
)


(defn cost-fn-factory [s t]
  (fn cost-fn [n res]
    (- 1 (/ (* -1  (measure (dc/spieltag s t)
                            (g/predict-result s t n)
                            metric-kickerpoints))
            36.0))))

(def experts [5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22])

(def init-state (m/->State  experts (vec (repeat (count experts) 1)) 0.5 (java.security.SecureRandom.)))

(defn expert-distribution [sa ta n]
  (loop [state init-state
         sps (dc/range-spieltage sa ta n)
         ]
    (let [[s t] (first sps)
          rest_sps (rest sps)
          new_state (m/step state (dc/spieltag s t) (cost-fn-factory s t))]
      (if (empty? rest_sps)
        new_state
        (recur new_state
               rest_sps)))))
