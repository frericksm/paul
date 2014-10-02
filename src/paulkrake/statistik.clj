(ns paulkrake.statistik
  (:require [paulkrake.glicko2 :as g]
            [paulkrake.bulibox :as b]))



(as-> (b/results1314) x
      ;(games-of x "FC Schalke 04")
      (map (fn [[_ _  _ h]] (Integer/parseInt h)) x)
      ((juxt frequencies count) x)
      ((fn [[f s]] (map (fn [[g c]] [g (double (/ c s))]) f)) x)
      (sort-by first x)
      )



(defn verteilung-tore [games heim?]
  (as-> games x
        (map (fn [[_ _  h g]] (if heim?
                               (Integer/parseInt h)
                               (Integer/parseInt g))) x)
        ((juxt frequencies count) x)
        ((fn [[f s]] (map (fn [[g c]] [g (double (/ c s))]) f)) x)
        (sort-by first x)
        )
  )
