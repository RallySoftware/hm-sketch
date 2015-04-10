(ns hmsketch.core-test
  (:require [clojure.test :refer :all]
            [hmsketch.core :as hms]))

(deftest new-sketches
  (is (not= {} (hms/new-sketch)))
  (is (not= (hms/new-sketch) (hms/new-sketch :res 100 :max 10000))))

(deftest add
  (is (not= (hms/new-sketch) (hms/add (hms/new-sketch) {"a" "b"} 10))))

(def x (hms/add
        (hms/add
         (hms/add
          (hms/add
           (hms/new-sketch)
           {"a" "b" "c" "d"} 10)
          {"a" "b" "c" "e"} 50)
         {"a" "c" "c" "d"} 10)
        {"a" "b" "c" "f"} 40))

(deftest serde
    (is (= x (hms/deserialize (hms/serialize x)))))

(deftest counts
  (is (= 1 (hms/getCount x {"a" "b" "c" "d"})))
  (is (= 3 (hms/getCount x {"a" "b"}))))

(deftest sketch
  (is (< 0 (count (keys (hms/sketch x {"a" "b" "c" "d"})))))
  (is (= 0 (count (keys (hms/sketch x {"a" "b" "c" "r"}))))))

(deftest incompatible
  (is (not (hms/incompatible x x)))
  (is (hms/incompatible x (hms/new-sketch :res 100))))

(deftest combine
  (is (= (hms/new-sketch) (hms/combine (hms/new-sketch) (hms/new-sketch))))
  (is (not= x (hms/combine x x))))

(deftest hist-remove
  (is (not= (hms/getCount x {"a" "b" "c" "d"}) (hms/getCount (hms/hist-remove x {"a" "b" "c" "d"}) {"a" "b" "c" "d"})))
  (is (= (hms/getCount x {"a" "b" "c" "r"}) (hms/getCount (hms/hist-remove x {"a" "b" "c" "r"}) {"a" "b" "c" "r"}))))

(deftest cdf
  (is (= '([10 1]) (hms/cdf x {"a" "b" "c" "d"})))
  (is (= '([10 1/3] [40 2/3] [50 1]) (hms/cdf x {"a" "b"}))))

(deftest reporting
  (is (= {0.50 10 0.75 40 0.90 40 0.95 40 0.99 40 "count" 3} (hms/report x {"a" "b"})))
  (is (= {0.50 10 0.75 40 0.90 40 0.95 40 0.99 40 "count" 3 "ksTest" 1.0} (hms/report x {"a" "b"} x {"a" "r"})))
  (is (= {0.50 10 0.75 40 0.90 40 0.95 40 0.99 40 "count" 3 "ksTest" 0.0} (hms/report x {"a" "b"} x {"a" "b"}))))
