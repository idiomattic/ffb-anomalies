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
      (>= abs-z 3.0) "VERY RARE"
      (>= abs-z 2.5) "RARE"
      (>= abs-z 2.0) "UNUSUAL"
      :else "NOTABLE")))

(defn- format-anomaly-description
  "Create a detailed description of the anomaly based on its type"
  [anomaly]
  (case (:type anomaly)
    :hot-streak
    (format "%s: HOT STREAK - Scored %.1f pts above their average (%.1f) over %d games"
            (:username anomaly)
            (:total-above-average anomaly)
            (:team-average anomaly)
            (:stretch-length anomaly))

    :cold-streak
    (format "%s: COLD STREAK - Scored %.1f pts below their average (%.1f) over %d games"
            (:username anomaly)
            (Math/abs (:total-below-average anomaly))
            (:team-average anomaly)
            (:stretch-length anomaly))

    :opponent-hot-streak
    (format "%s: BAD LUCK - Opponents scored %.1f pts above their averages over %d games"
            (:username anomaly)
            (:opponent-overperformance anomaly)
            (:stretch-length anomaly))

    :opponent-cold-streak
    (format "%s: GOOD LUCK - Opponents scored %.1f pts below their averages over %d games"
            (:username anomaly)
            (Math/abs (:opponent-underperformance anomaly))
            (:stretch-length anomaly))))

(defn- format-anomaly-details
  "Create detailed stats for the anomaly"
  [anomaly]
  (case (:type anomaly)
    :hot-streak
    (format "  Scored %.2f total (%.2f/week), averaging +%.2f above normal"
            (:total anomaly)
            (:average anomaly)
            (:avg-above-average anomaly))

    :cold-streak
    (format "  Scored %.2f total (%.2f/week), averaging %.2f below normal"
            (:total anomaly)
            (:average anomaly)
            (Math/abs (:avg-below-average anomaly)))

    :opponent-hot-streak
    (format "  Faced %.2f total (%.2f/week), opponents averaged +%.2f above their normal"
            (:total-against anomaly)
            (:average-against anomaly)
            (:avg-opponent-overperformance anomaly))

    :opponent-cold-streak
    (format "  Faced %.2f total (%.2f/week), opponents averaged %.2f below their normal"
            (:total-against anomaly)
            (:average-against anomaly)
            (Math/abs (:avg-opponent-underperformance anomaly)))))

(defn anomalies!
  [anomalies]
  (let [sorted-anomalies (->> anomalies
                              (filter #(>= (Math/abs (:z-score %)) (get-in config/store [:settings :min-z-score-of-interest])))
                              (sort-by :p-value))]
    (if (empty? sorted-anomalies)
      (println "No anomalous stretches found with the given p-value threshold.")
      (do
        (println (format "\nFound %d statistically significant anomalies:\n" (count sorted-anomalies)))
        (println (apply str (repeat 80 "=")))

        ;; Group by type for better organization
        (let [hot-streaks (filter #(= :hot-streak (:type %)) sorted-anomalies)
              cold-streaks (filter #(= :cold-streak (:type %)) sorted-anomalies)
              bad-luck (filter #(= :opponent-hot-streak (:type %)) sorted-anomalies)
              good-luck (filter #(= :opponent-cold-streak (:type %)) sorted-anomalies)]

          (when (seq hot-streaks)
            (println "\n🔥 HOT STREAKS (scoring above personal average):")
            (println (apply str (repeat 60 "-")))
            (doseq [anomaly (take 5 hot-streaks)]
              (println (format "[%s] %s"
                               (describe-anomaly-severity (:z-score anomaly))
                               (format-anomaly-description anomaly)))
              (println (format "  Weeks %s" (str/join ", " (:weeks anomaly))))
              (println (format-anomaly-details anomaly))
              (println (format "  Statistical significance: p=%.6f, z=%.2f"
                               (:p-value anomaly) (:z-score anomaly)))
              (when (>= (Math/abs (:z-score anomaly)) 3.0)
                (println (format "  ⚡ This is a %.1f-sigma event!" (Math/abs (:z-score anomaly)))))
              (println)))

          (when (seq cold-streaks)
            (println "\n🧊 COLD STREAKS (scoring below personal average):")
            (println (apply str (repeat 60 "-")))
            (doseq [anomaly (take 5 cold-streaks)]
              (println (format "[%s] %s"
                               (describe-anomaly-severity (:z-score anomaly))
                               (format-anomaly-description anomaly)))
              (println (format "  Weeks %s" (str/join ", " (:weeks anomaly))))
              (println (format-anomaly-details anomaly))
              (println (format "  Statistical significance: p=%.6f, z=%.2f"
                               (:p-value anomaly) (:z-score anomaly)))
              (when (>= (Math/abs (:z-score anomaly)) 3.0)
                (println (format "  ⚡ This is a %.1f-sigma event!" (Math/abs (:z-score anomaly)))))
              (println)))

          (when (seq bad-luck)
            (println "\n😭 BAD LUCK (opponents overperforming against you):")
            (println (apply str (repeat 60 "-")))
            (doseq [anomaly (take 5 bad-luck)]
              (println (format "[%s] %s"
                               (describe-anomaly-severity (:z-score anomaly))
                               (format-anomaly-description anomaly)))
              (println (format "  Weeks %s" (str/join ", " (:weeks anomaly))))
              (println (format-anomaly-details anomaly))
              (println (format "  Statistical significance: p=%.6f, z=%.2f"
                               (:p-value anomaly) (:z-score anomaly)))
              (when (>= (Math/abs (:z-score anomaly)) 3.0)
                (println (format "  ⚡ This is a %.1f-sigma event!" (Math/abs (:z-score anomaly)))))
              (println)))

          (when (seq good-luck)
            (println "\n🍀 GOOD LUCK (opponents underperforming against you):")
            (println (apply str (repeat 60 "-")))
            (doseq [anomaly (take 5 good-luck)]
              (println (format "[%s] %s"
                               (describe-anomaly-severity (:z-score anomaly))
                               (format-anomaly-description anomaly)))
              (println (format "  Weeks %s" (str/join ", " (:weeks anomaly))))
              (println (format-anomaly-details anomaly))
              (println (format "  Statistical significance: p=%.6f, z=%.2f"
                               (:p-value anomaly) (:z-score anomaly)))
              (when (>= (Math/abs (:z-score anomaly)) 3.0)
                (println (format "  ⚡ This is a %.1f-sigma event!" (Math/abs (:z-score anomaly)))))
              (println))))))))

(defn season-info!
  [{:keys [season-data stats]}]
  (let [{:keys [league-wide-std-dev team-stats]} stats]
    (println (format "\nSeason: %s" (:season season-data)))
    (println (format "Teams analyzed: %d" (count team-stats)))
    (println (format "League-wide deviation standard deviations:"))
    (println (format "  - Points For: %.2f (how much teams vary from their average)"
                     league-wide-std-dev))
    (println)
    (println "Team Averages:")
    (doseq [[username {:keys [mean std-dev]}] (sort-by #(- (get-in % [1 :mean])) team-stats)]
      (println (format "  %-20s: %.2f pts/week (σ=%.2f)" username mean std-dev)))))
