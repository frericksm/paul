(ns paulkrake.predict
  (:require [paulkrake.glicko2 :as g]
            [paulkrake.score :as s]
            [paulkrake.statistik :as st]))

(defn sample [data heim gast]
  (let [h-angriff-r  (get-in data [heim :angriff :rating])
        h-angriff-rd (get-in data [heim :angriff :rating-deviation])
        h-abwehr-r  (get-in data [heim :abwehr :rating])
        h-abwehr-rd (get-in data [heim :abwehr :rating-deviation])
        g-abwehr-r  (get-in data [gast :abwehr :rating])
        g-abwehr-rd (get-in data [gast :abwehr :rating-deviation])
        g-angriff-r  (get-in data [gast :angriff :rating])
        g-angriff-rd (get-in data [gast :angriff :rating-deviation])

        h-angriff-r-sampled  (st/sample-rating h-angriff-r h-angriff-rd)
        h-score (g/E-fn (g/mu h-angriff-r-sampled)
                        (g/mu g-abwehr-r)
                        (g/phi g-abwehr-rd))
        
        g-angriff-r-sampled  (st/sample-rating g-angriff-r g-angriff-rd)
        g-score (g/E-fn (g/mu g-angriff-r-sampled)
                        (g/mu h-abwehr-r)
                        (g/phi h-abwehr-rd))

        h-goals-float (s/goal-fn h-score)
        g-goals-float (s/goal-fn g-score)
        h-goals (Math/round h-goals-float)
        g-goals (Math/round g-goals-float)]
    [h-goals  g-goals]))



(defn sample-games
  [data games]
  (as-> games x
        (map (fn [[h g ]] (concat [h g] (sample data h g))) x)))

(defn pprint-samples [[h g h-goals g-goals]]
  (if (= (long h-goals) h-goals)
    (format "%24s - %24s   %d : %d" h g h-goals g-goals)
    (format "%24s - %24s   %.2f : %.2f" h g h-goals g-goals)))

(defn sample-games-pprint
  [data games]
  (as-> (sample-games data games) x
        (map pprint-samples x)))



(defn filter-samples-minimizing-chi-sq [samples]
  (let [samples-and-chi-sq (as-> samples x
                                 (map (fn [sg] {:sg sg :X-sq (st/chisq-test sg)}) x))
        min-chi-sq (apply min (map :X-sq samples-and-chi-sq))]
    (as-> samples-and-chi-sq x
          (filter (fn [{:keys [sg X-sq]}] (= X-sq min-chi-sq)  ) x)
          (map :sg x))))

(defn generate-samples [data games sample-size]
  (as-> (range sample-size) x
        (map (fn [i] (sample-games data games)) x)))

(defn select-max [hg-new gg-new counter-new
                        {:keys [hg gg counter] :as old}]
  (if (or (nil? old) (> counter-new counter))
    {:hg hg-new
     :gg gg-new
     :counter counter-new}
    old))

(defn predict-by-sample
  ([data games]
     (predict-by-sample data games 50 true))
  ([data games sample-size chi-sq?]
      (as-> (generate-samples data games sample-size) x
            (if chi-sq? (filter-samples-minimizing-chi-sq x) x))))

(defn- predict [data heim gast faktor-sigma]
  (let [h-angriff-r  (get-in data [heim :angriff :rating])
        h-angriff-rd (get-in data [heim :angriff :rating-deviation])
        h-abwehr-r  (get-in data [heim :abwehr :rating])
        h-abwehr-rd (get-in data [heim :abwehr :rating-deviation])
        g-abwehr-r  (get-in data [gast :abwehr :rating])
        g-abwehr-rd (get-in data [gast :abwehr :rating-deviation])
        g-angriff-r  (get-in data [gast :angriff :rating])
        g-angriff-rd (get-in data [gast :angriff :rating-deviation])
        h-score-min (g/E-fn (g/mu (- h-angriff-r
                                     (* faktor-sigma h-angriff-rd)))
                            (g/mu g-abwehr-r)
                            (g/phi g-abwehr-rd))
        h-score-max (g/E-fn (g/mu (+ h-angriff-r
                                     (* faktor-sigma h-angriff-rd)))
                            (g/mu g-abwehr-r)
                            (g/phi g-abwehr-rd))
        g-score-max (- 1.0  (g/E-fn (g/mu (- h-abwehr-r
                                             (* faktor-sigma h-abwehr-rd)))
                                    (g/mu g-angriff-r)
                                    (g/phi g-angriff-rd )))
        g-score-min (- 1.0  (g/E-fn (g/mu (+ h-abwehr-r
                                             (* faktor-sigma h-abwehr-rd)))
                                    (g/mu g-angriff-r)
                                    (g/phi g-angriff-rd )))]
    [[(s/goal-fn h-score-min) (s/goal-fn h-score-max)]
     [(s/goal-fn g-score-min) (s/goal-fn g-score-max)]]))


(defn predict-games
  ([data games]
     (predict-games data games 1.0))
  ([data games faktor-sigma]
     (as-> games x
           (map (fn [[h g ]]
                  (let [[[hmin hmax] [gmin gmax]] (predict data h g faktor-sigma)]
                    (format "%24s - %24s   [%.2f - %.2f] : [%.2f - %.2f]"
                            h g hmin hmax gmin gmax))) x))))
