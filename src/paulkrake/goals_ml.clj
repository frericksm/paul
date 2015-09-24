(ns paulkrake.goals-ml
  (:require [paulkrake.score :as s]
            [paulkrake.predict :as p]
            [paulkrake.goals :as g]
            [paulkrake.spielplan :as sp]
            [paulkrake.datacenter :as dc]
            [paulkrake.shots :as shots]
            [paulkrake.goals-per-shots :as gps]
            [paulkrake.shots :as shots]
            [incanter.distributions :as d]))

(defn score-to-toto
  "Takes a soccer result and translates it into a unique number"
  [g1 g2]
  (cond (= g1 g2) 2
        (< g1 g2) 1
        (> g1 g2) 3))

(defn score-to-label 
  "Takes a soccer result and translates it into a unique number"
  [g1 g2]
  (let [n (+ g1 g2)
        s (/ (* n (inc n)) 2)]
    (int  (+ s (inc g2)))))

(defn label-to-score 
  "Takes a label and translates it back into a soccer result"
  [label]
  (let [g1_0 (as-> (range 0 1000) x
               (filter (fn [i] (<= (score-to-label i 0) label)) x)
               (last x))
        n (/ (* g1_0 (inc g1_0)) 2)
        g2 (dec (- label n))
        g1  (- g1_0 g2)]
    [g1 g2]))

(defn format-2-decimals [x]
  (if (nil? x) (throw (java.lang.IllegalArgumentException. x)))
  (as-> x z
    (* 100 z)
    (int z)
    (/ z 100.0)))

(defn team-goals-features 
  "Bildet data-keyed-on-goals auf eine Map den Teams als Key"
  [data-keyed-on-goals]
  (let [max-goals (->> data-keyed-on-goals keys (apply max))
        teams (->> data-keyed-on-goals vals (map keys) (apply concat) (set))]
    (reduce 
     (fn [a t] 
       (assoc a t 
              (as-> (range 1 (inc max-goals)) y
                    (map (fn [i] (as-> (get-in data-keyed-on-goals [i t]) z
                                       (assoc z :goals i))) y)
                    (map (fn [{:keys [angriff abwehr]}]
                           (vector (:rating angriff)
                                   ;;(:rating-deviation angriff)
                                   (:rating abwehr)
                                   ;;(:rating-deviation abwehr)
                                   )) y)
                    (apply concat y))))
     {}
     teams)))

(defn data-keyed-on-goals 
  "Liefert Glicko2-Daten der letzten n Spieltage ab dem Spieltag [saison spieltag] als verschachtelte Map.
   Der Key in der Map z.B 1 enthält die Daten für das Spiel \"mehr als 1 Tor geschossen/verhindert\"
  {1 {\"Verein1\" {:angriff {:rating ...
                             :rating-deviation ...}
                   :abwehr  {:rating ...
                             :rating-deviation ...}}
      \"Verein2\" {:angriff {:rating ...
                             :rating-deviation ...}
                   :abwehr  {:rating ...
                             :rating-deviation ...}}
      ...}
   2 {\"Verein1\" {:angriff {:rating ...
                             :rating-deviation ...}
                   :abwehr  {:rating ...
                             :rating-deviation ...}}
      \"Verein2\" {:angriff {:rating ...
                             :rating-deviation ...}
                   :abwehr  {:rating ...
                             :rating-deviation ...}}
      ...}
   ...}"
  [saison spieltag n max-goals]
  (let [
        spieltage (dc/range-spieltage saison spieltag n)]
    (as-> (range 1 max-goals) x
          (reduce (fn [a i] (as-> i y
                                  (g/goals-to-score-fn-factory y)
                                  (g/goals-data spieltage y)
                                  (assoc a i y)))
                  {} x))))

(defn goals-7-17-34-features [saison spieltag]
  (let [max-goals 7]
    (merge-with concat 
                (team-goals-features (data-keyed-on-goals saison spieltag 7 max-goals))
                (team-goals-features (data-keyed-on-goals saison spieltag 17 max-goals))
                (team-goals-features (data-keyed-on-goals saison spieltag 34 max-goals)))))

  (defn spieltag-features [saison spieltag]
    (println "spieltag-features:" saison spieltag)
    (let [games (dc/spieltag saison spieltag)
          [saison-1 spieltag-1] (first (dc/range-spieltage saison spieltag 1))
          features (goals-7-17-34-features saison-1 spieltag-1) 
          ]
      (as-> games x
            (map (fn [[h g hg gg]]
                   (let [fh (get features h)
                         fg (get features g)]
                     (do (if (some nil? fh) (println h fh))
                         (if (some nil? fg) (println g fg))
                         (concat (vector hg gg) fh fg )))) x))))


(defn write-features-to-file [features out-file]
  (as-> features x 
    (map (fn [[hg gg & feat]]
           (cons (score-to-toto hg gg) (map format-2-decimals feat))) x) 
    (map (fn [v] (apply str (interpose "," v))) x)
    (interpose "\n" x)
    (apply str x)
    (spit out-file x)))


(defn make-features-file []
  (as-> (dc/range-spieltage 1516 3 0 853) x
    (map (fn [[s t]] (spieltag-features s t)) x)
    (apply concat x)
    (write-features-to-file x "resources/features.csv")))
