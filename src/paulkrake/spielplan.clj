(ns paulkrake.spielplan
  (:require [clojure.java.io :as io])
  )

(defn parse-ergebnis [e]
  (if (not (nil? e))
    (->> (clojure.string/split e #":")
         (map #(Integer/parseInt %)))))

(defn spieltag [& spieltage]
  (as-> spieltage y
        (map (fn [nr]
               (as-> (io/reader "resources/spielplan1415.csv" :encoding "Cp1252") x
                     (line-seq x)
                     (map #(clojure.string/split % #",") x)
                     (filter (fn [[t]] (= t (str nr))) x  )
                     (map (fn [[_ h g e t]]
                            (concat [h g]
                                    (parse-ergebnis e)
                                    (parse-ergebnis t))) x)))
             y)
        (apply concat y)
        (sort-by first y)))



