(ns fantasy-stats.calculate
  (:import (org.apache.commons.math3.stat.descriptive SummaryStatistics)
           (org.apache.commons.math3.distribution NormalDistribution)))

(defn summary-stats [numbers]
  (let [stats (SummaryStatistics.)]
    (doseq [n numbers]
      (.addValue stats (double n)))
    {:mean (.getMean stats)
     :std-dev (.getStandardDeviation stats)
     :variance (.getVariance stats)
     :count (.getN stats)}))

(defn team-season-stats
  "Calculate each team's mean and std-dev for the season"
  [parsed-matchups-by-week]
  (let [teams-data (reduce (fn [acc {:keys [matchups]}]
                             (reduce (fn [team-acc {:keys [username points-for]}]
                                       (update team-acc username (fnil conj []) points-for))
                                     acc
                                     matchups))
                           {}
                           parsed-matchups-by-week)]
    (reduce (fn [acc [username scores]]
              (assoc acc username (summary-stats scores)))
            {}
            teams-data)))

(defn calculate-relative-performances
  "For each matchup, calculate how much above/below average each team scored"
  [matchups team-stats]
  (mapv (fn [matchup]
          (let [user-mean (get-in team-stats [(:username matchup) :mean])
                opponent-mean (get-in team-stats [(:opponent matchup) :mean])]
            (assoc matchup
                   :points-for-deviation (when user-mean
                                           (- (:points-for matchup) user-mean))
                   :points-against-deviation (when opponent-mean
                                               (- (:points-against matchup) opponent-mean)))))
        matchups))

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
              :avg-against (/ (reduce + (map :points-against stretch)) stretch-length)
              ;; New fields for relative performance
              :total-for-deviation (reduce + (keep :points-for-deviation stretch))
              :total-against-deviation (reduce + (keep :points-against-deviation stretch))
              :avg-for-deviation (/ (reduce + (keep :points-for-deviation stretch)) stretch-length)
              :avg-against-deviation (/ (reduce + (keep :points-against-deviation stretch)) stretch-length)}))
         (range (- (count matchups) stretch-length -1)))))

(def ^:private standard-normal (NormalDistribution. 0.0 1.0))

(defn p-value
  "Calculate the p-value for a given value using normal distribution."
  [{:keys [value mean std-dev direction]}]
  (let [z-score (/ (- value mean) std-dev)]
    (case direction
      :high (- 1 (.cumulativeProbability standard-normal z-score))
      :low (.cumulativeProbability standard-normal z-score)
      :both (* 2 (min (.cumulativeProbability standard-normal z-score)
                      (- 1 (.cumulativeProbability standard-normal z-score)))))))

(defn anomalous-stretches
  "Find stretches that are statistically anomalous using the hybrid approach.
   Uses team's own average but league-wide variance for significance testing."
  [{:keys [username matchups league-wide-std-dev team-mean p-threshold]}]
  (reduce
   (fn [acc stretch-length]
     (let [stretches (find-consecutive-stretches matchups stretch-length)
           ;; Standard deviation for sum of n deviations
           std-dev-sum-for (* league-wide-std-dev (Math/sqrt stretch-length))
           std-dev-sum-against (* league-wide-std-dev (Math/sqrt stretch-length))]
       (reduce
        (fn [acc stretch]
          (let [;; For points-for: how much above/below own average
                p-value-for-high (when (pos? std-dev-sum-for)
                                   (p-value {:value (:total-for-deviation stretch)
                                             :mean 0  ;; Deviations should average to 0
                                             :std-dev std-dev-sum-for
                                             :direction :high}))
                p-value-for-low (when (pos? std-dev-sum-for)
                                  (p-value {:value (:total-for-deviation stretch)
                                            :mean 0
                                            :std-dev std-dev-sum-for
                                            :direction :low}))

                ;; For points-against: how much opponents overperformed
                p-value-against-high (when (pos? std-dev-sum-against)
                                       (p-value {:value (:total-against-deviation stretch)
                                                 :mean 0
                                                 :std-dev std-dev-sum-against
                                                 :direction :high}))
                p-value-against-low (when (pos? std-dev-sum-against)
                                      (p-value {:value (:total-against-deviation stretch)
                                                :mean 0
                                                :std-dev std-dev-sum-against
                                                :direction :low}))]
            (cond-> acc
              (and p-value-for-high (< p-value-for-high p-threshold))
              (conj {:username username
                     :type :hot-streak
                     :stretch-length stretch-length
                     :weeks (map :week (:stretch stretch))
                     :start-week (:week (first (:stretch stretch)))
                     :end-week (:week (last (:stretch stretch)))
                     :total (:total-for stretch)
                     :average (:avg-for stretch)
                     :team-average team-mean
                     :total-above-average (:total-for-deviation stretch)
                     :avg-above-average (:avg-for-deviation stretch)
                     :p-value p-value-for-high
                     :z-score (/ (:total-for-deviation stretch) std-dev-sum-for)})

              (and p-value-for-low (< p-value-for-low p-threshold))
              (conj {:username username
                     :type :cold-streak
                     :stretch-length stretch-length
                     :weeks (map :week (:stretch stretch))
                     :start-week (:week (first (:stretch stretch)))
                     :end-week (:week (last (:stretch stretch)))
                     :total (:total-for stretch)
                     :average (:avg-for stretch)
                     :team-average team-mean
                     :total-below-average (:total-for-deviation stretch)
                     :avg-below-average (:avg-for-deviation stretch)
                     :p-value p-value-for-low
                     :z-score (/ (:total-for-deviation stretch) std-dev-sum-for)})

              (and p-value-against-high (< p-value-against-high p-threshold))
              (conj {:username username
                     :type :opponent-hot-streak
                     :stretch-length stretch-length
                     :weeks (map :week (:stretch stretch))
                     :start-week (:week (first (:stretch stretch)))
                     :end-week (:week (last (:stretch stretch)))
                     :total-against (:total-against stretch)
                     :average-against (:avg-against stretch)
                     :opponent-overperformance (:total-against-deviation stretch)
                     :avg-opponent-overperformance (:avg-against-deviation stretch)
                     :p-value p-value-against-high
                     :z-score (/ (:total-against-deviation stretch) std-dev-sum-against)})

              (and p-value-against-low (< p-value-against-low p-threshold))
              (conj {:username username
                     :type :opponent-cold-streak
                     :stretch-length stretch-length
                     :weeks (map :week (:stretch stretch))
                     :start-week (:week (first (:stretch stretch)))
                     :end-week (:week (last (:stretch stretch)))
                     :total-against (:total-against stretch)
                     :average-against (:avg-against stretch)
                     :opponent-underperformance (:total-against-deviation stretch)
                     :avg-opponent-underperformance (:avg-against-deviation stretch)
                     :p-value p-value-against-low
                     :z-score (/ (:total-against-deviation stretch) std-dev-sum-against)}))))
        acc
        stretches)))
   []
   (range 2 (min 6 (inc (count matchups))))))
