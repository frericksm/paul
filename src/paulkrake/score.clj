(ns paulkrake.score
  (:require [paulkrake.glicko2 :as g]))

(defn initial-rating-data [vereine]
  (reduce (fn [a v] (assoc a v
                          {:abwehr {:rating 1500.0
                                    :rating-deviation 350.0
                                    :volatility 0.06}
                           :angriff {:rating 1500.0
                                     :rating-deviation 350.0
                                     :volatility 0.06}})) {} vereine))

(defn get-rating-data [data verein abwehr-or-sturm-keyword]
  (get-in data [verein abwehr-or-sturm-keyword]))

(defn update-rating-data [data verein abwehr-or-sturm-keyword rating-data]
  (assoc-in data [verein abwehr-or-sturm-keyword] rating-data))

(defn score-fn [number-of-goals]
  (let [goals-as-number (if (number? number-of-goals)
            number-of-goals
            (Integer/valueOf number-of-goals))]
    (as-> (range goals-as-number) x
          (map inc x)
          (map #(/ 1.0 (Math/pow 2 %)) x)
          (apply + x)
          )))

(defn abwehr-score-fn [number-of-goals]
  (- 1.0 (score-fn number-of-goals)))

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

(defn gegner [verein games]
  (map (fn [[heim gast heim-goals gast-goals]]
         (if (= verein heim) gast heim)) games))

(defn scores [verein games]
  (map (fn [[heim gast heim-goals gast-goals]]
         (let [abwehr-score (if (= verein heim)
                              (abwehr-score-fn gast-goals)
                              (abwehr-score-fn heim-goals))
               angriff-score (if (= verein heim)
                               (score-fn heim-goals)
                               (score-fn gast-goals))
               ]
           {:abwehr abwehr-score :angriff angriff-score})) games))

(defn exptected-score [r1 r2]
  (/ 1.0
     (+ 1 (Math/pow 10.0 (/  (- r2 r1) 400.0)))))

(defn predict [data heim gast]
  (let [h-angiff-r  (get-in data [heim :angriff :rating])
        h-angiff-rd (get-in data [heim :angriff :rating-deviation])
        h-abwehr-r  (get-in data [heim :abwehr :rating])
        h-abwehr-rd (get-in data [heim :abwehr :rating-deviation])
        g-abwehr-r  (get-in data [gast :abwehr :rating])
        g-abwehr-rd (get-in data [gast :abwehr :rating-deviation])
        g-angriff-r  (get-in data [gast :angriff :rating])
        g-angriff-rd (get-in data [gast :angriff :rating-deviation])
        ;h-score (exptected-score h-angiff-r g-abwehr-r)
        h-score (g/E-fn (g/mu h-angiff-r) (g/mu g-abwehr-r) (g/phi g-abwehr-rd))
        ;g-score (- 1.0  (exptected-score h-abwehr-r g-angriff-r))
        g-score (- 1.0  (g/E-fn (g/mu h-abwehr-r) (g/mu g-angriff-r) (g/phi g-angriff-rd )))]
    (println h-angiff-r  h-abwehr-r g-angriff-r g-abwehr-r )
    [(goal-fn h-score) (goal-fn g-score)]))
