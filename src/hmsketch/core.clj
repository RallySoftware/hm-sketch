(ns hmsketch.core
  (:require [clojure.set :as cs]
            [clojure.edn :as edn]))


(defn serialize ^bytes [clj]
  "out to bytes"
  (.getBytes  (pr-str clj)))

(defn deserialize [^bytes bts]
  "in from clj. Dangerous, potentially. It might read a lot of stuff...."
  (edn/read-string (String. bts)))

(defn new-sketch [& {:keys [res max] :or {res 10 max 10000}}]
  "Create a new sketch, takes optional resolution and maximum arguments"
  {:resolution res :max max :store {}})

(defn- parse-value [sketch value]
  "Project a value into the data structure"
  (let [res (:resolution sketch)
        box (:max sketch)]
    (min (max (- value (mod value res)) (- box)) box)))

(defn add [sketch locations value]
  "If locations is incomplete, it'll add anyways. Be careful"
  (let [store (:store sketch)
        val (parse-value sketch value)]
    ((apply comp
            (for [[x y] locations
                  :let [cur (get-in store [x y val])]]
              (fn [z] (assoc-in z [:store x y val] (if cur (inc cur) 1))))) sketch)))

(defn sketch [hms locations]
  "Take the sketch and get out the approximate histogram for the location"
  (let [pdfs (for [[x y] locations]
               (get-in hms [:store x y]))
        keysets (map #(apply hash-set (keys %)) pdfs)
        shrkeys (if (> (count keysets) 0) (apply cs/intersection keysets) nil)]
    (apply merge-with min (map #(select-keys % shrkeys) pdfs))))

(defn getCount [hms locations]
  "Get the count from a particular sketch location, approximate."
  (reduce + (vals (sketch hms locations))))

(defn getTotalCount [sketch]
  "get total count from a sketch"
  (let [ks (keys (:store sketch))
        hsts (vals ((:store sketch) (first ks)))]
    (reduce + (flatten (map vals hsts)))))

(defn incompatible [sketch1 sketch2]
  "check if two sketches have the different parameters"
  (or (not= (:resolution sketch1) (:resolution sketch2))
      (not= (:max sketch1) (:max sketch2))))

(defn combine [sketch1 sketch2]
  "Combine two sketches, it's possible to infer what to do among different resolutions and maxes, but for simplicity, we don't."
  (when (incompatible sketch1 sketch2)
    (throw (Exception. "Cannot combine sketches of different resolutions or bounds.")))
  ((apply comp
          (for [[x y] (:store sketch1)
                [z w] y
                [a b] w]
            (fn [q] (assoc-in q [:store x z a] (+ b (get-in sketch2 [:store x z a]))))
            )) sketch2))

(defn- hist-diff [hist1 hist2]
  "subtract hist2 from hist1"
  (merge-with - hist1 hist2))

(defn hist-remove [hms location]
  "Removes a hist from the sketch. If done in this manner, it should not ever result in negative values."
  (let [skch (sketch hms location)]
    ((apply comp (for [[x y] location]
                   (fn [z] (assoc-in z [:store x y] (hist-diff (get-in z [:store x y]) skch)))
                   )) hms)))

(defn cdf [hms location]
  "compute a cdf of the hms at the given location, returned as a list of pairs"
  (let [skch (sort (sketch hms location))
        count (max (reduce + (map second skch)) 1)
        ks (map first skch)
        vals (map second skch)
        csum (map #(/ % count) (reductions + vals))]
    (map vector ks csum)))


(defn fn-cdf [hms location]
  "compute a cdf function, this function will return the fraction of data <= to the given value"
  (let [proto (cdf hms location)
        outclj (into {} proto)
        keys (map first proto)
        vals (map second proto)]
    (if (> (count keys) 0)
      (fn [g] (get outclj (last (into [(first keys)]  (filter #(<= % g) keys)))))
      (fn [g] 0)
        )
    ))


(defn- fn-quantile [cdf]
  "Get a function which returns the nearest value less than or equal to a certain quantile"
  (let [keys (map first cdf)
        vals (map second cdf)
        outclj (into {} (map vector vals keys))]
    (if (> (count keys) 0)
      (fn [g] (get outclj (last (into [(first vals)] (filter #(<= % g) vals)))))
      (fn [g] 0))))

(defn quantiles [cdf & {:keys [quantiles] :or {quantiles [0.50 0.75 0.90 0.95 0.99]}}]
  "generate quantiles from a cdf"
  (let [qg (fn-quantile cdf)]
    (if (> (count cdf) 0)
      (into {} (map vector quantiles (map qg quantiles)))
     )))

(defn ks-test [sketch1 loc1 sketch2 loc2]
  "Rudimentary distributional similarity testing."
  (when (incompatible sketch1 sketch2)
      (throw (Exception. "Ks-testing on differing maxes or resolutions is possible, but not implemented.")))
  (let [cdf1 (fn-cdf sketch1 loc1)
        cdf2 (fn-cdf sketch2 loc2)
        smax (:max sketch1)
        res (:resolution sketch1)
        range (range (- smax) smax res)]
    (apply max (map #(Math/abs ^Double (- (cdf1 %) (cdf2 %))) range))))

(defn report
  ([sketch1 loc1]
   "output report for one histogram should be quantiles, counts, and mle stuff."
   (into  {"count" (getCount sketch1 loc1)}
          (quantiles (cdf sketch1 loc1))))
  ([sketch1 loc1 sketch2 loc2]
   "Generate the output of counts, p-values, and ks comparisons as appropriate"
   (let [quants (report sketch1 loc1)]
     (if (not (incompatible sketch1 sketch2))
       (into quants {"ksTest" (ks-test sketch1 loc1 sketch2 loc2)})
       quants))))
