(ns paulkrake.spielplan
  (:require [clojure.java.io :as io])
  )

(defn parse-ergebnis [e]
  (if (not (nil? e))
    (->> (clojure.string/split e #":")
         (map #(Integer/parseInt %)))))

(defn spieltag
  ([saison spieltag]
     (as-> (format "resources/spielplan%s.csv" (str saison)) x
           (io/reader x ) ;;:encoding "Cp1252" 
           (line-seq x)
           (map #(clojure.string/split % #",") x)
           (filter (fn [[t]] (= t (str spieltag))) x  )
           (map (fn [[_ h g e t]]
                  (concat [h g]
                          (parse-ergebnis e)
                          (parse-ergebnis t))) x)))
  ([spieltag] (spieltag 1415 spieltag)))



