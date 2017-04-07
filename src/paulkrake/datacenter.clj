(ns paulkrake.datacenter
  (:require [clojure.data.json :as json]
            [clojure.java.io]
            [clojure.edn]
            [paulkrake.debug :as d]))


(def all-kategories (list "passes_complete_percentage" "score_foot_against" "crosses" "passes_complete" "duels_lost_ground" "score" "duels_lost_header" "duels_won" "shot_assists" "tracking_fast_runs" "corner_kicks_left" "fouls_committed" "crosses_left" "card_red" "saves" "shots_inside_box" "fouls_suffered" "score_foot" "shots_header" "balls_touched_percentage" "shots_foot" "freekicks" "duels_won_percentage" "passes_failed" "crosses_right" "balls_touched" "score_header_against" "tracking_sprints" "shots" "corner_kicks" "duels_won_header" "score_header" "average_age" "duels_won_ground" "offsides" "passes_failed_percentage" "card_yellow_red" "shots_outside_box" "tracking_max_speed" "shots_on_goal" "tracking_average_speed" "score_penalty" "score_against" "duels_lost_percentage" "duels_lost" "tracking_distance" "team" "corner_kicks_right" "card_yellow" "score_penalty_against"))

(def kategories 
  ["passes_complete" "passes_complete_percentage" 
   "crosses" "crosses_left"
   "duels_won" 
   "shot_assists" 
   "tracking_fast_runs" 
   "corner_kicks_left" 
   "fouls_committed"  
   "card_red" 
   "saves" 
   "shots_inside_box" 
   "shots_header"
   "shots_foot"
   "balls_touched_percentage" 
   "freekicks" 
   "duels_won_percentage" 
   "passes_failed" 
   "crosses_right" 
   "balls_touched" 
   "tracking_sprints" "shots" "corner_kicks" "duels_won_header" "average_age" "duels_won_ground" "offsides" "passes_failed_percentage" "card_yellow_red" "shots_outside_box" "tracking_max_speed" "shots_on_goal" "tracking_average_speed"  "tracking_distance" "corner_kicks_right" "card_yellow"])

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
  (let [jahrhundert1 (if (as-> saison x 
                               (str x)
                               (.substring x 0 2)
                               (Integer/valueOf x) 
                               (< x 50))
                       "20" "19")
        jahrhundert2 (if (as-> saison x 
                               (str x)
                               (.substring x 2 4)
                               (Integer/valueOf x) 
                               (< x 50))
                       "20" "19")
        name (format "%s%s/%s%s"
                     jahrhundert1
                     (apply str (take 2 (str saison)))
                     jahrhundert2
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

(defn stat-data-remote [saison spieltag-nr]
  (println "stat-data-remote" saison spieltag-nr)
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
                       hg       (team-stats-by-match match-id home-id) 
                       ag       (team-stats-by-match match-id away-id)]
                   (vector home away hg ag)))
         x)))

(defn stat-data-file [saison]
  (clojure.java.io/file (format "resources/stat-%s.txt" (str saison))))

(defn stat-data-local [p-saison p-spieltag-nr]
  #_(println "stat-data-local" p-saison p-spieltag-nr)
  (as-> (stat-data-file p-saison) x
    (if (.exists x) 
      (as-> x y
        (line-seq (clojure.java.io/reader y))
        (map clojure.edn/read-string y)
        (filter (fn [{:keys [saison spieltag] :as m}]
                  (and (= (str p-saison) saison)
                       (= p-spieltag-nr spieltag))) y)
        (map :data y)
        (first y)))))

(defn stat-data-unmemoized [saison spieltag-nr]
  (as-> (stat-data-file saison) x
    (if (not (nil? x)) (stat-data-local saison spieltag-nr))
    (if (nil? x) (stat-data-remote saison spieltag-nr) x)))

(def stat-data (memoize stat-data-unmemoized))

(defn teams [saison spieltag-nr]
  (as-> (stat-data saison spieltag-nr) x
    (map (fn [[home away _ _]]
             (vector home away)) x)
    (apply concat x)
    (set x)))


(defn data [saison spieltag-nr kategorie]
  (as-> (stat-data saison spieltag-nr) x
    (map (fn [[home away hg ag]] 
           (let [hg_new (as-> hg y
                          (get y kategorie)
                          (Double/valueOf y))
                 ag_new (as-> ag y
                          (get y kategorie)
                          (Double/valueOf y))]
             (vector home away hg_new ag_new)))
         x))) 

(defn score [saison spieltag-nr]
  (as-> (data saison spieltag-nr "score") x
    (map (fn [[h g hg gg]] (vector h g (int hg) (int gg))) x)))

(defn spieltag [saison spieltag-nr]
  (score saison spieltag-nr))

(defn shots-on-goal [saison spieltag-nr]
  (data saison spieltag-nr "shots"))

(defn passes-complete-percentage [saison spieltag-nr]
  (data saison spieltag-nr "passes_complete_percentage"))

(defn gauss-klammer [number]
  (if (< number 0)
    (if (< number (int number)) (dec (int number)) (int number))
    (int number)))

(defn spieltag-add 
  "Berechnet den n-ten Spieltag nach dem Spieltag [saison t]"
  [saison t n]
  {:pre  [(<= 1 t 34)]
   :post [(<= 1 (second %) 34)]}
  (let [tmod34 (dec t)
        new_saison_1 (as-> saison x
                           (str x)
                           (.substring x 0 2)
                           (Integer/valueOf x))
        new_saison_2 (+ new_saison_1 (gauss-klammer (/ (+ tmod34 n) 34)))
        new_saison_3 (format "%02d%02d" 
                             (mod new_saison_2 100) 
                             (mod (inc new_saison_2) 100))
        new_tag_1  (mod (+ tmod34 n) 34) 
        new_tag_2  (inc new_tag_1)]
    [new_saison_3 new_tag_2]))

(defn spieltag-before [s t]
  (spieltag-add s t -1))

(defn spieltag-after [s t]
  (spieltag-add s t 1))

(defn range-spieltage 
  "Berechnet eine Range von Spieltagen"
  ([saison spieltag n1 n2]
   (as-> (range n1 n2) x
     (map (fn [i] (spieltag-add saison spieltag (* -1 i))) x)
     (reverse x)
     ))
  ([saison spieltag n]
   (range-spieltage saison spieltag 1 (+ 1 n))))
   

(defn sample-of-spieltage [s t n m]
  (as->  (range-spieltage s t n) x
    (shuffle x)
    (take m x)))

(defn dump [saison spieltag-bis out-dir]
  (let [st (range-spieltage saison spieltag-bis 0 spieltag-bis)
        st-d-local (reduce (fn [a [s t]] (assoc a [s t] (stat-data-local s t))) 
                           {} st)]
  (with-open [wtr (clojure.java.io/writer 
                   (format "%s/stat-%s.txt" out-dir (str saison)))]
    (as-> st x
      (map (fn [[s t]] 
             (if (not (nil? (get st-d-local [s t])))
               (hash-map :saison s :spieltag t :data (get st-d-local [s t]))
               (hash-map :saison s :spieltag t :data (stat-data-remote s t)))) x)
      (doseq [line x] (.write wtr (str (pr-str line) "\n"))))
    )))

(defn min-max-spieltag []
   (let [saison-0001 "0001"
         min-spieltag (as-> (range (* 34 51)) y
                        (map (fn [i] (spieltag-add saison-0001 1 (* -1 i))) y)
                        (filter (fn [[s t]] (and (.exists (stat-data-file s))
                                                 (stat-data-local s t))) y)
                        (last y))
         max-spieltag (as-> (range (* 34 49)) y
                        (map (fn [i] (spieltag-add saison-0001 1 i)) y)
                        (filter (fn [[s t]] (and (.exists (stat-data-file s))
                                                 (stat-data-local s t))) y)
                        (last y))]
     (vector min-spieltag max-spieltag)))

     
