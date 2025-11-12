(ns fantasy-stats.fantasy-stats
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [clojure.string :as str]
            [clojure.edn :as edn])
  (:import (org.apache.commons.math3.stat.descriptive SummaryStatistics)
           (org.apache.commons.math3.distribution NormalDistribution))
  (:gen-class))

(defn read-data []
  (let [all-files (.list (io/file "resources/"))]
    (mapv #(edn/read-string (slurp (io/resource %))) all-files)))

(defn parse-matchups
  "Takes a week's matchups, returning a sequence of maps containing :username, :points-for, and :points-against."
  [matchups roster-id->username]
  (-> (group-by :matchup_id matchups)
      (update-vals (fn [[roster-a roster-b]]
                     [{:username (get roster-id->username (:roster_id roster-a))
                       :opponent (get roster-id->username (:roster_id roster-b))
                       :points-for (:points roster-a)
                       :points-against (:points roster-b)}
                      {:username (get roster-id->username (:roster_id roster-b))
                       :opponent (get roster-id->username (:roster_id roster-a))
                       :points-for (:points roster-b)
                       :points-against (:points roster-a)}]))
      (vals)
      (flatten)))

(defn calculate-stats [numbers]
  (let [stats (SummaryStatistics.)]
    (doseq [n numbers]
      (.addValue stats (double n)))
    {:mean (.getMean stats)
     :std-dev (.getStandardDeviation stats)
     :variance (.getVariance stats)}))

(defn find-consecutive-stretches
  "Find all consecutive stretches of matchups of a given length."
  [matchups stretch-length]
  (when (>= (count matchups) stretch-length)
    (map (fn [start-idx]
           (let [stretch (subvec (vec matchups) start-idx (+ start-idx stretch-length))]
             {:start-index start-idx
              :end-index (+ start-idx stretch-length -1)
              :stretch stretch
              :total-for (reduce + (map :points-for stretch))
              :total-against (reduce + (map :points-against stretch))
              :avg-for (/ (reduce + (map :points-for stretch)) stretch-length)
              :avg-against (/ (reduce + (map :points-against stretch)) stretch-length)}))
         (range (- (count matchups) stretch-length -1)))))

(def ^:private standard-normal (NormalDistribution. 0.0 1.0))

(defn calculate-p-value
  "Calculate the p-value for a given value using normal distribution."
  [value mean std-dev direction]
  (let [z-score (/ (- value mean) std-dev)]
    (case direction
      :high (- 1 (.cumulativeProbability standard-normal z-score))
      :low (.cumulativeProbability standard-normal z-score)
      :both (* 2 (min (.cumulativeProbability standard-normal z-score)
                      (- 1 (.cumulativeProbability standard-normal z-score)))))))

(defn find-anomalous-stretches
  "Find stretches that are statistically anomalous based on p-value threshold."
  [username matchups all-points-for all-points-against p-threshold]
  (let [stats-for (calculate-stats all-points-for)
        stats-against (calculate-stats all-points-against)]
    (reduce
     (fn [acc stretch-length]
       (let [stretches (find-consecutive-stretches matchups stretch-length)
             ;; For multi-game stretches, we need to adjust the standard deviation
             ;; The variance of a sum is n * variance (assuming independence)
             std-dev-sum-for (* (:std-dev stats-for) (Math/sqrt stretch-length))
             std-dev-sum-against (* (:std-dev stats-against) (Math/sqrt stretch-length))
             mean-sum-for (* (:mean stats-for) stretch-length)
             mean-sum-against (* (:mean stats-against) stretch-length)]
         (reduce
          (fn [acc stretch]
            (let [p-value-for-high (calculate-p-value (:total-for stretch) mean-sum-for std-dev-sum-for :high)
                  p-value-for-low (calculate-p-value (:total-for stretch) mean-sum-for std-dev-sum-for :low)
                  p-value-against-high (calculate-p-value (:total-against stretch) mean-sum-against std-dev-sum-against :high)
                  p-value-against-low (calculate-p-value (:total-against stretch) mean-sum-against std-dev-sum-against :low)]
              (cond-> acc
                (< p-value-for-high p-threshold)
                (conj {:username username
                       :type :points-for-high
                       :stretch-length stretch-length
                       :weeks (map :week (:stretch stretch))
                       :start-week (:week (first (:stretch stretch)))
                       :end-week (:week (last (:stretch stretch)))
                       :total (:total-for stretch)
                       :average (:avg-for stretch)
                       :p-value p-value-for-high
                       :z-score (/ (- (:total-for stretch) mean-sum-for) std-dev-sum-for)})

                (< p-value-for-low p-threshold)
                (conj {:username username
                       :type :points-for-low
                       :stretch-length stretch-length
                       :weeks (map :week (:stretch stretch))
                       :start-week (:week (first (:stretch stretch)))
                       :end-week (:week (last (:stretch stretch)))
                       :total (:total-for stretch)
                       :average (:avg-for stretch)
                       :p-value p-value-for-low
                       :z-score (/ (- (:total-for stretch) mean-sum-for) std-dev-sum-for)})

                (< p-value-against-high p-threshold)
                (conj {:username username
                       :type :points-against-high
                       :stretch-length stretch-length
                       :weeks (map :week (:stretch stretch))
                       :start-week (:week (first (:stretch stretch)))
                       :end-week (:week (last (:stretch stretch)))
                       :total (:total-against stretch)
                       :average (:avg-against stretch)
                       :p-value p-value-against-high
                       :z-score (/ (- (:total-against stretch) mean-sum-against) std-dev-sum-against)})

                (< p-value-against-low p-threshold)
                (conj {:username username
                       :type :points-against-low
                       :stretch-length stretch-length
                       :weeks (map :week (:stretch stretch))
                       :start-week (:week (first (:stretch stretch)))
                       :end-week (:week (last (:stretch stretch)))
                       :total (:total-against stretch)
                       :average (:avg-against stretch)
                       :p-value p-value-against-low
                       :z-score (/ (- (:total-against stretch) mean-sum-against) std-dev-sum-against)}))))
          acc
          stretches)))
     []
     (range 2 (min 6 (inc (count matchups)))))))

(defn describe-anomaly-severity
  "Describe how severe an anomaly is based on z-score"
  [z-score]
  (let [abs-z (Math/abs z-score)]
    (cond
      (>= abs-z 3.5) :EXTREME
      (>= abs-z 3.0) :VERY
      (>= abs-z 2.5) :RARE
      (>= abs-z 2.0) :UNUSUAL
      :else :NOTABLE)))


(defn -main
  [& args]
  (let [p-threshold (if (empty? args)
                      0.01  ; Default to 1% significance level
                      (let [parsed (parse-double (first args))]
                        (if (nil? parsed)
                          (do
                            (println "Error: Invalid p-value threshold. Please provide a valid decimal number.")
                            (System/exit 1))
                          parsed)))]

    (println (format "Using p-value threshold: %s (finding events rarer than %.1f%%)"
                     p-threshold
                     (* 100 p-threshold)))
    (println)

    (doseq [season-data (read-data)]
      (let [{matchups-by-week :matchups roster-id->username :roster-id->username} season-data
            parsed-matchups-by-week (reduce
                                     (fn [acc {:keys [season week matchups]}]
                                       (conj acc {:week week
                                                  :season season
                                                  :matchups (parse-matchups matchups roster-id->username)}))
                                     []
                                     matchups-by-week)
            all-points-for (mapcat (fn [{:keys [matchups]}]
                                     (map :points-for matchups))
                                   parsed-matchups-by-week)
            all-points-against (mapcat (fn [{:keys [matchups]}]
                                         (map :points-against matchups))
                                       parsed-matchups-by-week)
            season-stats-for (calculate-stats all-points-for)
            season-stats-against (calculate-stats all-points-against)
            league-members (vals roster-id->username)
            matchups-by-username (reduce
                                  (fn [acc username]
                                    (assoc acc username (->> parsed-matchups-by-week
                                                             (mapv
                                                              (fn [{:keys [season week matchups]}]
                                                                (merge {:season season
                                                                        :week week}
                                                                       (some #(when (= username (:username %)) (dissoc % :username))
                                                                             matchups))))
                                                             (filter :opponent)
                                                             (map-indexed (fn [i el] (assoc el :matchup-index i))))))
                                  {}
                                  league-members)
            all-anomalies (reduce (fn [acc [username matchups]]
                                    (if-let [anomalies (seq (find-anomalous-stretches username matchups all-points-for all-points-against p-threshold))]
                                      (concat acc anomalies)
                                      acc))
                                  []
                                  matchups-by-username)]

        (println (format "Season: %s" (:season season-data)))
        (println (format "Points For - Mean: %.2f, Std Dev: %.2f" (:mean season-stats-for) (:std-dev season-stats-for)))
        (println (format "Points Against - Mean: %.2f, Std Dev: %.2f" (:mean season-stats-against) (:std-dev season-stats-against)))
        (println)

        (let [sorted-anomalies (sort-by :p-value all-anomalies)]
          (if (empty? sorted-anomalies)
            (println "No anomalous stretches found with the given p-value threshold.")
            (do
              (println (format "Found %d anomalous stretches:" (count sorted-anomalies)))
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
                (println)))))))))
