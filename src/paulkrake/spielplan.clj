(ns paulkrake.spielplan
  (:require [clojure.java.io :as io])
  )


(defn spieltag [nr]
  (as-> (io/reader "resources/spielplan1415.csv" :encoding "Cp1252") x
        (line-seq x)
        (map #(clojure.string/split % #",") x)
        (filter (fn [[t]] (= t (str nr))) x  )
        (map (fn [[_ h g e]] (concat [h g] (if (not (nil? e)) (clojure.string/split e #":")))) x)
        )
  )


