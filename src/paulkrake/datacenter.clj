(ns paulkrake.datacenter
  (:require [net.cgrand.enlive-html :as html]
            [paulkrake.score :as s]
            [paulkrake.glicko2 :as g]))

(def datacenter-uri "http://www.sport1.de/dynamic/datencenter/sport/teamstatistik-torschuesse/fussball/bundesliga-20%s-20%s/_r26201_/_m%s_/")

(def ver2ver
  {"Bayer Leverkusen" "Bayer 04 Leverkusen"       
   "VfL Wolfsburg" "VfL Wolfsburg"                
   "1. FSV Mainz 05" "FSV Mainz 05"
   "Werder Bremen" "Werder Bremen"                
   "VfB Stuttgart" "VfB Stuttgart"                
   ;;"Eintracht Braunschweig" ;Absteiger
   "Borussia Dortmund" "Borussia Dortmund"            
   "Hertha BSC" "Hertha BSC Berlin"
   "Bayern München" "FC Bayern München"            
   "Bor. Mönchengladbach" "Borussia Mönchengladbach"     
   "Eintracht Frankfurt" "Eintracht Frankfurt"
   "Hannover 96"  "Hannover 96"
   "1899 Hoffenheim" "1899 Hoffenheim"
   "SC Freiburg" "Sport-Club Freiburg"
   ;;"1. FC Nürnberg"   ;Absteiger
   "FC Schalke 04" "FC Schalke 04"
   "FC Augsburg" "FC Augsburg 1907"             
   "Hamburger SV" "Hamburger SV"
   "SC Paderborn 07" "SC Paderborn 07" ;Aufsteiger
   "1. FC Köln" "1. FC Köln"  ;Aufsteiger
   })

(defn map-inverse [m]
  (as-> m x
        (map (fn [[k v]] [v k]) x)
        (into {} x)
        )
  )

(defn shots-on-goal [saison spieltag]
  (as-> datacenter-uri x
        (format x
                (apply str (take 2 (str saison)))
                (apply str (drop 2 (str saison)))
                (str spieltag))
        (html/html-resource (java.net.URL. x))
        (html/select x #{[:.wfb_team_name]
                         [:#wfb_teamstats-container :.wfb_team_value]})
        (map :content x)
        (map last x)
        (map #(clojure.string/trim %) x)
        (drop 1 x)
        (partition 2 2 x)
        (map (fn [[team shots]] [( get ver2ver team) shots]) x)
        ))
