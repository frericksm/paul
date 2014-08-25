(ns paulkrake.bulibox
  (:require [net.cgrand.enlive-html :as html]
            [paulkrake.score :as s]))

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

(defn games-of [verein]
  (as-> (results1314) x
        (filter (fn [[h g e]] (contains? #{h g } verein )) x)))

(defn rating1314 []
  (let [vereine (vereine1314)
        initial-scoring (s/initial-scoring vereine)
        verein2games (reduce (fn [a v] (assoc a v (games-of v))) {} vereine)
        ]))
