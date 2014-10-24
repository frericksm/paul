(ns paulkrake.data
  (:require [paulkrake.spielplan :as s]
            [paulkrake.bulibox :as b]
            [clojure.edn :as e]
            [paulkrake.datacenter :as d]))


(defn store-rating-data [saison spieltag data]
  (spit (format  "resources/rating-%s-%s.edn" saison spieltag)
        (pr-str data)
        :encoding "UTF-8"))

(defn read-rating-data [saison spieltag]
  (as-> (slurp (format  "resources/rating-%s-%s.edn" saison spieltag) :encoding "UTF-8")
        x
        (e/read-string x)))


(defn stats [saison spieltag-nr]
        (let [st (s/spieltag saison spieltag-nr)
              sog (d/shots-on-goal saison spieltag-nr)]
          (as-> st x
                (map (fn [[heim gast hg gg]]
                       (let [h_sog (get sog heim)
                             g_sog (get sog gast)
                             h_p (if (= h_sog 0) 0 (int (* 100 (/ hg h_sog))))
                             g_p (if (= g_sog 0) 0 (int (* 100 (/ gg g_sog))))
                             ]
                         (vector heim gast hg gg h_sog g_sog h_p g_p) )) x))))
