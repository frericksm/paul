(ns paulkrake.datacenter
  (:require [net.cgrand.enlive-html :as html]
            [paulkrake.score :as s]
            [paulkrake.glicko2 :as g]
            [clojure.data.json :as json]))

(def kategorien ["tore" "gegentore" "ballbesitz" "zweikaempfe"
                 "laufstrecke" "karten" "torschuesse" "top-speed"
                 "gefoult" "gefoult-worden" "ecken" "sprints" ])

(defn to-clj-unmemoized [uri]
  (->> uri slurp json/read-str ))

(def to-clj (memoize to-clj-unmemoized))

(defn competitions []
  (as-> "http://sportsapi.sport1.de/competition/co12" x
        (to-clj x)))

(defn team-stats-by-match [match-id team-id]
  (as-> "http://sportsapi.sport1.de/team-stats-by-match/ma%s/te%s" x
        (format x match-id team-id)
        (to-clj x)))

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

(defn spieltag [saison spieltag-nr]
  (as-> (season-by-competition saison) x
        (get x "id")
        (matches-by-season x spieltag-nr)
        (get x "round")
        (first x)
        (get x "match")
        (map (fn [m] (let [home     (get-in m ["home" "name"])
                          away     (get-in m ["away" "name"])
                          finished (= "yes" (get m "finished"))
                          hg       (if finished (as-> m y
                                                      (get y "match_result")
                                                      (filter #(and (= "home" (get % "place"))
                                                                    (= "0" (get % "match_result_at"))) y)
                                                      (map #(get % "match_result") y)
                                                      (first y)))
                          ag       (if finished (as-> m y
                                                      (get y "match_result")
                                                      (filter #(and (= "away" (get % "place"))
                                                                    (= "0" (get % "match_result_at"))) y)
                                                      (map #(get % "match_result") y)
                                                      (first y)))]
                      (vector home away hg ag)))
             x)))


(defn map-inverse [m]
  (as-> m x
        (map (fn [[k v]] [v k]) x)
        (into {} x)
        )
  )




(defn data [saison spieltag kategorie])


(defn shots-on-goal [saison spieltag]
  (data saison spieltag "torschuesse"))

(defn tore [saison spieltag]
  (data saison spieltag "tore"))

(defn gegentore [saison spieltag]
  (data saison spieltag "gegentore"))
