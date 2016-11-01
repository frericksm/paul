(ns paulkrake.mlprep
  (:require [paulkrake.datacenter :as dc]
            [paulkrake.goals :as g]))

(defn alle-vereine [])

(defn input-vector [[s t :as spieltag]]
  (dc/spieltag s t ))
