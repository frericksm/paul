(ns paulkrake.datacenter
  (:require [net.cgrand.enlive-html :as html]
            [paulkrake.score :as s]
            [paulkrake.glicko2 :as g]))

(def kategorien ["tore" "gegentore" "ballbesitz" "zweikaempfe"
                 "laufstrecke" "karten" "torschuesse" "top-speed"
                 "gefoult" "gefoult-worden" "ecken" "sprints" ])

(def datacenter-uri "http://www.sport1.de/dynamic/datencenter/sport/teamstatistik-%s/fussball/bundesliga-20%s-20%s/_r26201_/_m%s_/")

(def ergebnisse  "http://www.sport1.de/dynamic/datencenter/sport/ergebnisse/fussball/bundesliga-20%s-20%s/_r26201_/_m%s_/")

(defn to-num [x]
  (cond (number? x) x
        (= "-" x)   x
        true        (Integer/parseInt x)))

(defn split-ergebnis [ergebnis]
  (->> (clojure.string/split ergebnis #":")
       (map to-num)))

(defn spieltag [saison spieltag-nr]
  (as-> ergebnisse x
        (format x
                (apply str (take 2 (str saison)))
                (apply str (drop 2 (str saison)))
                (str spieltag-nr))
        (html/html-resource (java.net.URL. x))
        (html/select x #{[:.wfb_football_table :tr :td :a]})
        (map :content x)        
        (flatten x)
        (filter #(not (nil? %)) x)
        (map (fn [r] (cond
                     (string? r) (clojure.string/trim r)
                     (map? r)   (:content r)
                     )) x)
        (flatten x)
        (filter #(not (empty? %)) x)
        (reduce (fn [a d] (if (or (.startsWith d ":")
                                 (nil? a)
                                 (.endsWith a ":"))
                           (str a d )
                           (str a "," d)
                           ))
                nil x)
        (clojure.string/split x #",")
        (if (contains? (set x) "-:-") (partition 3 3 x) (partition 4 4 x))
        (map (fn [[h g ergebnis halbzeitstand]] 
               (concat [h g] 
                       (split-ergebnis ergebnis))) x)))


(defn map-inverse [m]
  (as-> m x
        (map (fn [[k v]] [v k]) x)
        (into {} x)
        )
  )




(defn data [saison spieltag kategorie]
  
  (as-> datacenter-uri x
        (format x
                kategorie
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
        (map (fn [[team shots]] [team (Integer/parseInt shots)]) x)
        (into {} x)
        ))


(defn shots-on-goal [saison spieltag]
  (data saison spieltag "torschuesse"))

(defn tore [saison spieltag]
  (data saison spieltag "tore"))

(defn gegentore [saison spieltag]
  (data saison spieltag "gegentore"))
