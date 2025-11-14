(ns fantasy-stats.calculate
  (:import (org.apache.commons.math3.stat.descriptive SummaryStatistics)
           (org.apache.commons.math3.distribution NormalDistribution)))

(defn summary-stats [numbers]
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
  "Find stretches that are statistically anomalous based on p-value threshold."
  [{:keys [username matchups all-points p-threshold]}]
  (let [{:keys [std-dev mean]} (summary-stats all-points)]
    (reduce
     (fn [acc stretch-length]
       (let [stretches (find-consecutive-stretches matchups stretch-length)
             ;; For multi-game stretches, we need to adjust the standard deviation
             ;; The variance of a sum is n * variance (assuming independence)
             std-dev-sum (* std-dev (Math/sqrt stretch-length))
             mean-sum (* mean stretch-length)]
         (reduce
          (fn [acc stretch]
            (let [p-value-for-high (p-value {:value (:total-for stretch)
                                             :mean mean-sum
                                             :std-dev std-dev-sum
                                             :direction :high})
                  p-value-for-low (p-value {:value (:total-for stretch)
                                            :mean mean-sum
                                            :std-dev std-dev-sum
                                            :direction :low})
                  p-value-against-high (p-value {:value (:total-against stretch)
                                                 :mean mean-sum
                                                 :std-dev std-dev-sum
                                                 :direction :high})
                  p-value-against-low (p-value {:value (:total-against stretch)
                                                :mean mean-sum
                                                :std-dev std-dev-sum
                                                :direction :low})]
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
                       :z-score (/ (- (:total-for stretch) mean-sum) std-dev-sum)})

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
                       :z-score (/ (- (:total-for stretch) mean-sum) std-dev-sum)})

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
                       :z-score (/ (- (:total-against stretch) mean-sum) std-dev-sum)})

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
                       :z-score (/ (- (:total-against stretch) mean-sum) std-dev-sum)}))))
          acc
          stretches)))
     []
     (range 2 (min 6 (inc (count matchups)))))))
