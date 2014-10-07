(ns paulkrake.statistik
  (:require [paulkrake.glicko2 :as g]
            [incanter.stats :as is]
            [paulkrake.bulibox :as b]
            [paulkrake.spielplan :as sp]))


(defn chisq-class
  [goals] 
  (cond (<= goals 1) goals
        (>  goals 1) 2))

(defn chisq-frequencies [sample-data]
  (reduce (fn [a g] (update-in a [(chisq-class g)] inc))
          {0 0 1 0 2 0} ;; inital frequencies map
          sample-data))

(defn frequencies-goals [games select-fn]
  (as-> games x
        (map select-fn x)
        (chisq-frequencies x)
        (sort-by first x)))

(defn frequencies-goals-home [games]
  (frequencies-goals games #(nth % 2)))

(defn frequencies-goals-guest [games]
  (frequencies-goals games #(nth % 3)))

(defn probabilities-goals [games select-fn]
  (let [f (frequencies-goals games select-fn)
        sum (apply + (vals f))]
    (map (fn [[k v]] [k (double (/ v sum))]) f)))

(defn probabilities-goals-home [games]
  (probabilities-goals games #(nth % 2)))

(defn probabilities-goals-guest [games]
  (probabilities-goals games #(nth % 3)))

(defn sample-rating
  "Return a sample value of the normal distribution to mu and sigma"
  [mu sigma]
  (is/sample-normal 1 :mean mu :sd sigma)
  )


(defn probs-home-goals-per-gameday []
  (as-> (concat (b/results1314) (sp/spieltag 1 2 3 4 5 6 7)) x 
        (probabilities-goals-home x)))

(defn probs-guest-goals-per-gameday []
  (as-> (concat (b/results1314) (sp/spieltag 1 2 3 4 5 6 7)) x 
        (probabilities-goals-guest x)))


(defn chisq-test [sample-games]
  (let [x-freq-home  (->> sample-games
                          frequencies-goals-home
                          (map second))
        x-freq-guest (->> sample-games
                          frequencies-goals-guest
                          (map second))
        cs-home      (is/chisq-test :x x-freq-home
                                    :probs (->> (probs-home-goals-per-gameday)
                                                (map second)))

        X-sq-home (:X-sq cs-home)

        cs-guest     (is/chisq-test :x x-freq-guest
                                    :probs (->> (probs-home-goals-per-gameday)
                                                (map second)))

        X-sq-guest (:X-sq cs-guest)]
    ;[cs-home cs-guest]
    (Math/sqrt (+  (* X-sq-home X-sq-home)
                   (* X-sq-guest X-sq-guest)))))
