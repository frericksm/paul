(ns paulkrake.bulibox
  (:require [net.cgrand.enlive-html :as html]
            [paulkrake.score :as s]
            [paulkrake.glicko2 :as g]))

(def ergebnisse-bl1314 "http://www.bulibox.de/spieltage/B100151.html")


(defn fetch-url [url]
  (html/html-resource (java.net.URL. url)))


(defn create-csv [url]
  (as-> (html/select (fetch-url ergebnisse-bl1314) #{[:table.tab :tr :td ]})   x
        (partition 4 4 x)
        (map (fn [p] (->> p  (take 2) (map :content))) x)
        (map (fn [[[p] [e]]] (concat (clojure.string/split p #" - ") [e])) x)
        (map (fn [[h g e]] (format "%s,%s,%s" h g (apply str (drop-last 1 e)))) x)
        (interpose "\n" x)
        (apply str x)
        (spit "resources/results1314.csv" x :encoding "Cp1252")
        ))

(defn results1314 []
  (as-> (slurp "resources/results1314.csv" :encoding "Cp1252") x
        (clojure.string/split x #"\n" )
        (map (fn [r] (clojure.string/split r #"," )) x)
        (map (fn [[h g e]] (concat [h g] (clojure.string/split e #":") )) x)
        ))

(defn vereine1314 []
  (as-> (results1314) x
        (map (fn [[h]] h) x)
        (set x)))



(defn ratings1314 []
  (let [vereine (vereine1314)
        rating-data (s/initial-rating-data vereine)
        games (results1314)
        verein2games (reduce (fn [a v] (assoc a v (s/games-of games v))) {} vereine)
        ]
    (as-> vereine x
          (map (fn [v] [v (s/new-vereins-rating rating-data v (get verein2games v))]) x)
          (into {} x))))

(def v2v {"Bayer 04 Leverkusen" "SV Bayer 04 Leverkusen"       
          "VfL Wolfsburg""VfL Wolfsburg"                
          "1. FSV Mainz 05" "FSV Mainz 05"                 
          "SV Werder Bremen" "Werder Bremen"                
          "VfB Stuttgart" "VfB Stuttgart"                
          ;"Eintracht Braunschweig" ;Absteiger
          "Borussia Dortmund" "Borussia Dortmund"            
          "Hertha BSC" "Hertha BSC Berlin"            
          "FC Bayern München" "FC Bayern München"            
          "Borussia Mönchengladbach" "Borussia Mönchengladbach"     
          "Eintracht Frankfurt" "Eintracht Frankfurt"
          "Hannover 96"  "Hannover 96"
          "1899 Hoffenheim" "1899 Hoffenheim"
          "Sport-Club Freiburg" "Sport-Club Freiburg"
          ;"1. FC Nürnberg"   ;Absteiger
          "FC Schalke 04" "FC Schalke 04"
          "FC Augsburg" "FC Augsburg 1907"             
          "Hamburger SV" "Hamburger SV"
          "SC Paderborn 07" "SC Paderborn 07" ;Aufsteiger
          "1. FC Köln" "1. FC Köln"  ;Aufsteiger
          })

(defn ratings1415 []
  (let [r1314 (ratings1314)]
    (as->  v2v x
           (map (fn [[a b]] [a  (get r1314 a  s/start-rating-data )]) x)
           (into {} x))))
