(ns paulkrake.mwua)

;; see: https://courses.cs.washington.edu/courses/cse521/10wi/kale-thesis-chap2.pdf
;; Multiplicative Weights Update method


(defrecord State [experts weights learning-rate random])

(defn normalize [weights]
  (let [sum (apply + weights)]
    (vec (map (fn [w] (/ w sum)) weights))))

(defn draw
  "Zieht aus einem State mit der Wahrscheinlichkeitsverteilung weights
  einen Experten"
  [{:keys [weights random experts ]}]
  (let [normalized_weights (normalize weights)
        p (.nextDouble random)]
    (loop [index 0
           partial_sum_weights 0]
      (let [new_partial_sum_weights (+ partial_sum_weights
                                       (nth normalized_weights index))]
        (if (<= p new_partial_sum_weights)
          (nth experts index)
          (recur (inc index) 
                 new_partial_sum_weights))))))

(defn step [{:keys [experts weights learning-rate] :as state} outcome cost-fn]
  (as-> weights x
    (normalize x)
    (map-indexed (fn [i w]
                   (let [costs (cost-fn (nth experts i) outcome)
                         factor (if (>= costs 0)
                                  (Math/pow (- 1 learning-rate) costs)
                                  (Math/pow (+ 1 learning-rate) (* -1 costs)))]
                     (* w factor))) x)
    (vec x)
    (normalize x)
    (assoc state :weights x)))

(defn experts-sorted-by-weight
  [{:keys [experts weights learning-rate] :as state}]
  (as-> (map (fn [e w] (vector e w)) experts  weights) x
    (sort-by second x)
    (reverse x)))

