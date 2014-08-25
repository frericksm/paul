(ns paulkrake.score)

(defn initial-scoring [vereine]
  (reduce (fn [a v] (assoc a v
                     {:abwehr {:rating 1500.0
                               :rating-deviation 350.0
                               :volatility 0.06}
                      :sturm {:rating 1500.0
                              :rating-deviation 350.0
                              :volatility 0.06}})) {} vereine))

(defn scoring-data [data verein abwehr-or-sturm-keyword]
  (get-in data [verein abwehr-or-sturm-keyword]))

(defn score-fn [number-of-goals]
  (let [goals-as-number (if (number? number-of-goals)
            number-of-goals
            (Integer/valueOf number-of-goals))]
    (as-> (range goals-as-number) x
          (map inc x)
          (map #(/ 1.0 (Math/pow 2 %)) x)
          (apply + x)
          )))


(defn goal-fn [score]
  (as-> (range) x
        (partition 2 1 x)
        (filter (fn [[a b]]
                  (and (<= (score-fn a) score )
                       (< score (score-fn b)))) x)
        (map (fn [[a b]]
               (+ a (*  (/ (- score (score-fn a)) 
                           (- (score-fn b) (score-fn a))) 
                        (- b a)))) x)
        (first x)))
