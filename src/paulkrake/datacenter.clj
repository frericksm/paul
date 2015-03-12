(ns paulkrake.datacenter
  (:require [clojure.data.json :as json]))

(defn to-clj-unmemoized [uri]
  (->> uri slurp json/read-str ))

(def to-clj (memoize to-clj-unmemoized))

(defn competitions []
  (as-> "http://sportsapi.sport1.de/competition/co12" x
        (to-clj x)))

(defn team-stats-by-match [match-id team-id]
  (as-> "http://sportsapi.sport1.de/team-stats-by-match/ma%s/te%s" x
        (format x match-id team-id)
        (to-clj x)
        (first x)))

(defn season-by-competition [saison]
  (let [name (format "20%s/20%s" 
                (apply str (take 2 (str saison)))
                (apply str (drop 2 (str saison))))]
    (as-> "http://sportsapi.sport1.de/seasons-by-competition/co12" x
          (to-clj x)
          (get x "season")
          (filter #(= name (get % "name")) x)
          (first x))))

(defn rounds-by-season [saison]
  (as-> (format "http://sportsapi.sport1.de/rounds-by-season/se%s" (str saison)) x
        (to-clj x)))

(defn matches-by-season
  ([]
     (to-clj "http://sportsapi.sport1.de/matches-by-season/co12"))

  ([saison-id]
      (as-> (format "http://sportsapi.sport1.de/matches-by-season/co12/se%s" (str saison-id)) x
            (to-clj x)))

  ([saison-id spieltag]
     (as-> (format "http://sportsapi.sport1.de/matches-by-season/co12/se%s/md%s"
                   (str saison-id)
                   (str spieltag)) x
            (to-clj x))))

(defn to-num [x]
  (cond (number? x) x
        (= "-" x)   x
        true        (Integer/parseInt x)))

(defn split-ergebnis [ergebnis]
  (->> (clojure.string/split ergebnis #":")
       (map to-num)))

(defn map-inverse [m]
  (as-> m x
        (map (fn [[k v]] [v k]) x)
        (into {} x)))

(defn data [saison spieltag-nr kategorie]
  (as-> (season-by-competition saison) x
        (get x "id")
        (matches-by-season x spieltag-nr)
        (get x "round")
        (first x)
        (get x "match")
        (map (fn [m] (let [home     (get-in m ["home" "name"])
                          away     (get-in m ["away" "name"])
                          home-id  (get-in m ["home" "id"]) 
                          away-id  (get-in m ["away" "id"])
                          match-id (get m "id")
                          hg       (as-> (team-stats-by-match match-id home-id) y
                                     (get y kategorie)
                                     (Double/valueOf y))
                          ag       (as-> (team-stats-by-match match-id away-id) y
                                     (get y kategorie)
                                     (Double/valueOf y))]
                      (vector home away hg ag)))
             x)))

(defn spieltag [saison spieltag-nr]
  (data saison spieltag-nr "score"))

(defn score [saison spieltag-nr]
  (data saison spieltag-nr "score"))

(defn shots-on-goal [saison spieltag-nr]
  (data saison spieltag-nr "shots"))

(defn passes-complete-percentage [saison spieltag-nr]
  (data saison spieltag-nr "passes_complete_percentage"))

