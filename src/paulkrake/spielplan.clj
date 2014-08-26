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


(def v2v {"Bayer 04 Leverkusen" "SV Bayer 04 Leverkusen"       
          "VfL Wolfsburg""VfL Wolfsburg"                
          "1. FSV Mainz 05" "FSV Mainz 05"                 
          "SV Werder Bremen" "Werder Bremen"                
          "VfB Stuttgart" "VfB Stuttgart"                
          ;"Eintracht Braunschweig"
          "Borussia Dortmund" "Borussia Dortmund"            
          "Hertha BSC" "Hertha BSC Berlin"            
          "FC Bayern München" "FC Bayern München"            
          "Borussia Mönchengladbach" "Borussia Mönchengladbach"     
          "Eintracht Frankfurt" "Eintracht Frankfurt"
          "Hannover 96"  "Hannover 96"
          "1899 Hoffenheim" "1899 Hoffenheim"
          "Sport-Club Freiburg" "Sport-Club Freiburg"
          ;"1. FC Nürnberg"
          "FC Schalke 04" "FC Schalke 04"
          "FC Augsburg" "FC Augsburg 1907"             
          "Hamburger SV" "Hamburger SV"
          "SC Paderborn 07" "SC Paderborn 07"
          "1. FC Köln" "1. FC Köln"
          })
