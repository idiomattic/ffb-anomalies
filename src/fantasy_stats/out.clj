(ns fantasy-stats.out
  (:require [clojure.string :as str]
            [fantasy-stats.config :as config]))

(defn p-threshold! [p-threshold]
  (println (format "Using p-value threshold: %s (finding events rarer than %.1f%%)"
                   p-threshold
                   (* 100 p-threshold))))

(defn- describe-anomaly-severity
  "Describe how severe an anomaly is based on z-score"
  [z-score]
  (let [abs-z (Math/abs z-score)]
    (cond
      (>= abs-z 3.5) "EXTREME"
      (>= abs-z 3.0) "VERY"
      (>= abs-z 2.5) "RARE"
      (>= abs-z 2.0) "UNUSUAL"
      :else "NOTABLE")))

(defn anomalies!
  [anomalies]
  (let [sorted-anomalies (->> anomalies
                              (filter #(>= (Math/abs (:z-score %)) (get-in config/store [:settings :min-z-score-of-interest])))
                              (sort-by :p-value))]
    (if (empty? sorted-anomalies)
      (println "No anomalous stretches found with the given p-value threshold.")
      (do
        (println (format "Found %d anomalous stretches:"
                         (count sorted-anomalies)))
        (println)
        (doseq [anomaly (take 20 sorted-anomalies)]
          (println (format "[%s] %s: %s streak (weeks %s)"
                           (describe-anomaly-severity (:z-score anomaly))
                           (:username anomaly)
                           (case (:type anomaly)
                             :points-for-high "Extremely HIGH scoring"
                             :points-for-low "Extremely LOW scoring"
                             :points-against-high "Faced extremely HIGH opponents"
                             :points-against-low "Faced extremely LOW opponents")
                           (str/join ", " (:weeks anomaly))))
          (println (format "  Total: %.2f (avg %.2f/week), p-value: %.6f, z-score: %.2f"
                           (:total anomaly)
                           (:average anomaly)
                           (:p-value anomaly)
                           (:z-score anomaly)))
          (when (>= (Math/abs (:z-score anomaly)) 3.0)
            (println (format "  ^ This is a %.1f-sigma event!" (Math/abs (:z-score anomaly)))))
          (println))))))

(defn season-info!
  [{:keys [season-data stats-for stats-against]}]
  (println (format "Season: %s" (:season season-data)))
  (println (format "Points For - Mean: %.2f, Std Dev: %.2f" (:mean stats-for) (:std-dev stats-for)))
  (println (format "Points Against - Mean: %.2f, Std Dev: %.2f" (:mean stats-against) (:std-dev stats-against))))
