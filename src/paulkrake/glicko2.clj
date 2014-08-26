(ns paulkrake.glicko2)

;http://www.glicko.net/glicko/glicko2.pdf

(def tau 1.0)

(def epsilon 0.000001)

(defn mu [r] (/ (- r 1500.0) 173.7178 ))

(defn phi [rd] (/ rd 173.7178))

(defn rating [mu] (+ 1500.0 (* 173.7178 mu)))

(defn rating-deviation  [phi] (* 173.7178 phi))

(defn g
  [phi]
  (/ 1.0
     (Math/sqrt (+ 1 (* 3.0 (/ (* phi phi) (* Math/PI Math/PI)))))))

(defn E-fn [mu, mu_j, phi_j]
  (/ 1.0
     (+ 1 (Math/exp (* -1.0 (g phi_j) (- mu mu_j))))))

(defn v-summand [mu mu_i phi_i]
  (let [e (E-fn mu mu_i phi_i)]
    (* (g phi_i) (g phi_i) e (- 1.0 e)))
  )
(defn v-fn [mu mus phis]
  (/ 1.0 (apply + (map (partial v-summand mu) mus phis))))

(defn delta-summand [mu mu_i phi_i s_i]
  (* (g phi_i) (- s_i (E-fn mu mu_i phi_i))))

(defn delta-fn [v mu mus phis s]
  (* v (apply + (map (partial delta-summand mu) mus phis s))))



(defn sandwich-f [tau delta phi v a x]
  (let [delta2 (* delta delta)
        phi2   (* phi phi)
        ex     (Math/pow Math/E x)
        tau2   (* tau tau)]

    (- (/  (* ex (- delta2 phi2 v ex) )
           (* 2.0 (+ phi2 v ex) (+ phi2 v ex)))
       (/ (- x a) tau2))))

(defn fn-A [sigma] (Math/log (* sigma sigma)))

(defn fn-B [a sigma phi delta v]
  (let [a      (Math/log (* sigma sigma))
        delta2 (* delta delta)
        phi2   (* phi phi)
        ]
    (if (> delta2 (+ phi2 v))
      (Math/log (- delta2 phi2 v))
      (as-> (range) x
            (map inc x)
            (map (fn [k] (- a (* k tau))) x)
            (filter (fn [term] (>= (sandwich-f tau delta phi v a term) 0)) x)
            (first x)))))


(defn new-sigma [sigma phi delta v]
  (let [a      (fn-A sigma)]
    (loop
        [A      a
         B      (fn-B a sigma phi delta v)
         fA     (sandwich-f tau delta phi v a A)
         fB     (sandwich-f tau delta phi v a B)]
      ;(println (format "A: %s B: %s fA: %s fB %s" A B fA fB))
      (if (<= (Math/abs (- B A)) epsilon)
        (Math/pow Math/E (/ A 2.0))
        (let [C (+ A (* (- A B) fA (/ 1.0 (- fB fA))))
              fC (sandwich-f tau delta phi v a C)
              ]
          (recur
           (if (< (* fA fB) 0) B A)
           C
           (if (< (* fA fB) 0) fB (/ fA 2.0))
           fC))))))

(defn phi-star [phi sigma-strich]
  (Math/sqrt ( + (* phi phi) (* sigma-strich sigma-strich)) )
  )

(defn new-phi [v phi-star]
  (/ 1.0 (Math/sqrt (+  (/ 1.0 (* phi-star phi-star)) (/ 1.0 v)  ))))

(defn new-mu-summand [mu mu_i phi_i s_i]
  (* (g phi_i) (- s_i (E-fn mu mu_i phi_i))))

(defn new-mu [mu phi-strich mus phis s]

  (+ mu (* phi-strich phi-strich
           (apply + (map (partial new-mu-summand mu)  mus phis s)))))


(defn adjust-rating [old-rating old-rating-deviation old-volatility
                     opponents-rating oppenents-rating-deviation game-scores ]
  (let [mu1 (mu old-rating)
        phi1 (phi old-rating-deviation)
        mus (if (coll? opponents-rating)
              (map mu opponents-rating)
              [(mu opponents-rating)])
        phis (if (coll? oppenents-rating-deviation)
               (map phi oppenents-rating-deviation)
               [(phi oppenents-rating-deviation)])
        scores (if (coll? game-scores) game-scores [game-scores])
        sigma old-volatility
        v (v-fn mu1 mus phis)
        delta (delta-fn v mu1 mus phis scores)
        sigma-strich (new-sigma sigma phi1 delta v)
        phi-strich (new-phi v (phi-star phi1 sigma-strich))
        mu-strich (new-mu mu1 phi-strich mus phis scores)]
    {:rating (rating mu-strich)
     :rating-deviation (rating-deviation phi-strich)
     :volatility sigma-strich}))
