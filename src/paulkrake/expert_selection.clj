(ns paulkrake.expert-selection
  (:require [paulkrake.datacenter :as dc]
            [paulkrake.goals :as g]
            [paulkrake.mwua :as m]))

(defn cost-fn-factory [s t]
  (fn cost-fn [n res]
    (- 1 (/ (* -1  (measure (dc/spieltag s t)
                            (g/predict-result s t n)
                            metric-kickerpoints))
            36.0))))

#_(def experts [5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22])
(def experts [12 13 14 15 16 17 18 19 20 21 22])

(def init-state (m/->State  experts (vec (repeat (count experts) 1)) 0.5 (java.security.SecureRandom.)))

(defn expert-distribution 
  ([sa ta n]
   (expert-distribution sa ta n init-state))
  ([sa ta n state]
   (loop [state state
          sps (dc/range-spieltage sa ta n)
          ]
     (let [[s t] (first sps)
           rest_sps (rest sps)
           new_state (m/step state (dc/spieltag s t)
                             (cost-fn-factory s t))]
       (if (empty? rest_sps)
         new_state
         (recur new_state
                rest_sps))))))

;;(expert-distribution 1617 27 900)
(def optistate 
  (m/->State [12 13 14 15 16 17 18 19 20 21 22]
             [0.011000381577724726
              0.002497687136383716
              0.029938418498140627
              0.07992598836427936
              0.3801942161457011
              0.03560298028991777
              0.0246950216758353
              0.0484481795549259
              0.08971388861251939
              0.1318555965299911
              0.16612764161458096]
             0.5
             (java.security.SecureRandom.)))

(defn expert-predict [s t]
  (let [state_last (read-state s t)
        [sn tn] s_next (spieltag-add s t 1)
        state_next (expert-distribution sn tn state_last)
        i (draw state_next)
        n (nth (:experts s) i)]
    (println n)
    (predict-result 1617 27 n)))
