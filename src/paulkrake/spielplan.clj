(ns paulkrake.spielplan
  (:require [clojure.java.io :as io]
            [paulkrake.datacenter :as d])
  )

(defn parse-ergebnis [e]
  (if (not (nil? e))
    (->> (clojure.string/split e #":")
         (map #(Integer/parseInt %)))))

(defn spieltag
  ([saison spieltag-nr]
     (as-> (format "resources/spielplan%s.csv" (str saison)) x
           (io/reader x ) ;;:encoding "Cp1252" 
           (line-seq x)
           (map #(clojure.string/split % #",") x)
           (filter (fn [[t]] (= t (str spieltag-nr))) x  )
           (map (fn [[_ h g e t]]
                  (concat [h g]
                          (parse-ergebnis e)
                          (parse-ergebnis t))) x)))
  ([spieltag-nr] (spieltag 1415 spieltag-nr)))



(defn spieltag-shots [saison spieltag-nr]
        (let [st (spieltag saison spieltag-nr)
              sog (d/shots-on-goal saison spieltag-nr)]
          (as-> st x
                (map (fn [[heim gast hg gg]]
                       (let [h_sog (get sog heim)
                             g_sog (get sog gast)]
                         (vector heim gast h_sog g_sog))) x))))

(defn rounded [x]
  (as-> x z (* 100.0 z) (Math/round z) (/ z 100.0)))

(defn spieltag-treffer-pro-shots [saison spieltag-nr]
        (let [st (spieltag saison spieltag-nr)
              sog (d/shots-on-goal saison spieltag-nr)]
          (as-> st x
                (map (fn [[heim gast hg gg]]
                       (let [h_sog (get sog heim)
                             g_sog (get sog gast)
                             h_p (if (= h_sog 0) 0 (rounded (/ hg h_sog)))
                             g_p (if (= g_sog 0) 0 (rounded (/ gg g_sog)))
                             ]
                         (vector heim gast h_p g_p) )) x))))
