(ns paulkrake.debug
  (:require [clojure.string]))

(defn debug [msg x] (println msg x) x)
