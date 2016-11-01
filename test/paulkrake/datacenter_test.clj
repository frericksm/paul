(ns paulkrake.datacenter-test
  (:require [clojure.test :refer :all]
            [paulkrake.datacenter :refer :all]))

(deftest spieltag-add-in-saison
  (testing "Wrap saison."
    (is (= ["0001" 2] (spieltag-add "0001" 1 1)))
    (is (= ["0001" 17] (spieltag-add "0001" 20 -3)))))

(deftest spieltag-add-test
  (testing "Wrap saison."
    (is (= ["0102" 1] (spieltag-add "0001" 1 34)))
    (is (= ["9900" 1] (spieltag-add "0001" 1 -34)))
    (is (= ["9900" 34] (spieltag-add "0001" 1 -1)))))
