(ns paulkrake.data
  (:require [clojure.edn :as e]))


(defn store-rating-data [saison spieltag data]
  (spit (format  "resources/rating-%s-%s.edn" saison spieltag)
        (pr-str data)
        :encoding "UTF-8"))

(defn read-rating-data [saison spieltag]
  (as-> (slurp (format  "resources/rating-%s-%s.edn" saison spieltag)
               :encoding "UTF-8")
        x
        (e/read-string x)))



