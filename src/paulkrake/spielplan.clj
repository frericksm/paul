(ns paulkrake.spielplan
  (:require [clojure.java.io :as io])
  )


(defn spieltag [nr]
  (as-> (io/reader "resources/spielplan1415.csv" :encoding "Cp1252") x
        (line-seq x)
        (map #(clojure.string/split % #",") x)
        (filter (fn [[t]] (= t (str nr))) x  ) )
  )

(defn score-fn [tor]
        (let [t (if (number? tor) tor (Integer/valueOf tor))]
          (as-> (range t ) x
            (map inc x)
            (map #(/ 1.0 (Math/pow 2 %)) x)
            (apply + x)
            )))
