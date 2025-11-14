(ns fantasy-stats.calculate-test
  (:require [clojure.test :refer [deftest testing is]]
            [fantasy-stats.calculate :as calc]))

;; ==================== HELPER DATA ====================

(def simple-scores
  "Simple test data with round numbers for easy verification"
  [100.0 110.0 90.0 120.0 80.0])

(def simple-matchups
  "Simple matchup data for testing consecutive stretches"
  [{:week 1 :username "Alice" :opponent "Bob" :points-for 100.0 :points-against 90.0}
   {:week 2 :username "Alice" :opponent "Charlie" :points-for 110.0 :points-against 95.0}
   {:week 3 :username "Alice" :opponent "Dave" :points-for 90.0 :points-against 100.0}
   {:week 4 :username "Alice" :opponent "Eve" :points-for 120.0 :points-against 85.0}
   {:week 5 :username "Alice" :opponent "Frank" :points-for 80.0 :points-against 105.0}])

(def parsed-matchups-by-week
  "Sample week-by-week matchup data for testing team-season-stats"
  [{:week 1
    :season "2024"
    :matchups [{:username "Alice" :points-for 100.0}
               {:username "Bob" :points-for 90.0}
               {:username "Charlie" :points-for 95.0}]}
   {:week 2
    :season "2024"
    :matchups [{:username "Alice" :points-for 110.0}
               {:username "Bob" :points-for 85.0}
               {:username "Charlie" :points-for 105.0}]}
   {:week 3
    :season "2024"
    :matchups [{:username "Alice" :points-for 90.0}
               {:username "Bob" :points-for 100.0}
               {:username "Charlie" :points-for 110.0}]}])

;; ==================== SUMMARY STATS TESTS ====================

(deftest test-summary-stats-basic
  (testing "summary-stats calculates correct statistics"
    (let [stats (calc/summary-stats simple-scores)]

      (testing "Mean calculation"
        (is (= 100.0 (:mean stats))
            "Mean of [100, 110, 90, 120, 80] should be 100"))

      (testing "Count is correct"
        (is (= 5 (:count stats))
            "Should count all 5 values"))

      (testing "Standard deviation is positive"
        (is (> (:std-dev stats) 0)
            "Standard deviation should be positive for varying data"))

      (testing "Variance equals std-dev squared"
        (is (< (Math/abs (- (:variance stats)
                            (* (:std-dev stats) (:std-dev stats))))
               0.0001)
            "Variance should equal standard deviation squared")))))

(deftest test-summary-stats-known-values
  (testing "summary-stats with known statistical values"
    (let [data [2.0 4.0 4.0 4.0 5.0 5.0 7.0 9.0]  ; mean=5, std-dev=2
          stats (calc/summary-stats data)]

      (testing "Mean matches expected value"
        (is (= 5.0 (:mean stats))))

      (testing "Standard deviation matches expected value"
        (is (< (Math/abs (- (:std-dev stats) 2.0)) 0.01)
            "Standard deviation should be approximately 2.0")))))

(deftest test-summary-stats-edge-cases
  (testing "summary-stats edge cases"

    (testing "Single value"
      (let [stats (calc/summary-stats [100.0])]
        (is (= 100.0 (:mean stats)))
        (is (= 1 (:count stats)))
        (is (Double/isNaN (:std-dev stats))
            "Standard deviation of single value should be NaN")))

    (testing "Identical values"
      (let [stats (calc/summary-stats [100.0 100.0 100.0])]
        (is (= 100.0 (:mean stats)))
        (is (= 0.0 (:std-dev stats))
            "Standard deviation of identical values should be 0")))))

;; ==================== TEAM SEASON STATS TESTS ====================

(deftest test-team-season-stats
  (testing "team-season-stats aggregates scores correctly"
    (let [team-stats (calc/team-season-stats parsed-matchups-by-week)]

      (testing "All teams are present"
        (is (= 3 (count team-stats)))
        (is (contains? team-stats "Alice"))
        (is (contains? team-stats "Bob"))
        (is (contains? team-stats "Charlie")))

      (testing "Alice's statistics are correct"
        (let [alice-stats (get team-stats "Alice")]
          (is (= 100.0 (:mean alice-stats))
              "Alice's mean: (100 + 110 + 90) / 3 = 100")
          (is (= 3 (:count alice-stats)))))

      (testing "Bob's statistics are correct"
        (let [bob-stats (get team-stats "Bob")]
          ;; Bob: 90, 85, 100 -> mean = 91.67
          (is (< (Math/abs (- (:mean bob-stats) 91.666)) 0.01))
          (is (= 3 (:count bob-stats)))))

      (testing "Each team has required fields"
        (doseq [[username stats] team-stats]
          (is (contains? stats :mean) (str username " should have :mean"))
          (is (contains? stats :std-dev) (str username " should have :std-dev"))
          (is (contains? stats :variance) (str username " should have :variance"))
          (is (contains? stats :count) (str username " should have :count")))))))

;; ==================== CALCULATE RELATIVE PERFORMANCES TESTS ====================

(deftest test-calculate-relative-performances
  (testing "calculate-relative-performances computes deviations correctly"
    (let [team-stats {"Alice" {:mean 100.0 :std-dev 10.0}
                      "Bob" {:mean 90.0 :std-dev 8.0}
                      "Charlie" {:mean 95.0 :std-dev 5.0}}
          matchups [{:username "Alice" :opponent "Bob"
                     :points-for 110.0 :points-against 95.0}
                    {:username "Alice" :opponent "Charlie"
                     :points-for 85.0 :points-against 100.0}]
          result (calc/calculate-relative-performances matchups team-stats)]

      (testing "Deviations are calculated"
        (is (= 2 (count result))))

      (testing "First matchup deviations"
        (let [m1 (first result)]
          (is (= 10.0 (:points-for-deviation m1))
              "Alice scored 110, mean is 100, so deviation is +10")
          (is (= 5.0 (:points-against-deviation m1))
              "Bob scored 95, his mean is 90, so deviation is +5")))

      (testing "Second matchup deviations"
        (let [m2 (second result)]
          (is (= -15.0 (:points-for-deviation m2))
              "Alice scored 85, mean is 100, so deviation is -15")
          (is (= 5.0 (:points-against-deviation m2))
              "Charlie scored 100, his mean is 95, so deviation is +5")))

      (testing "Original matchup data is preserved"
        (let [m1 (first result)]
          (is (= "Alice" (:username m1)))
          (is (= "Bob" (:opponent m1)))
          (is (= 110.0 (:points-for m1)))
          (is (= 95.0 (:points-against m1))))))))

(deftest test-calculate-relative-performances-missing-stats
  (testing "Handle missing team stats gracefully"
    (let [team-stats {"Alice" {:mean 100.0}}
          matchups [{:username "Alice" :opponent "Unknown"
                     :points-for 110.0 :points-against 95.0}]
          result (calc/calculate-relative-performances matchups team-stats)]

      (testing "Deviation is nil when opponent stats missing"
        (is (= 10.0 (:points-for-deviation (first result))))
        (is (nil? (:points-against-deviation (first result))))))))

;; ==================== CONSECUTIVE STRETCHES TESTS ====================

(deftest test-find-consecutive-stretches-count
  (testing "find-consecutive-stretches finds correct number of stretches"

    (testing "2-game stretches"
      (let [stretches (calc/find-consecutive-stretches simple-matchups 2)]
        (is (= 4 (count stretches))
            "5 games should yield 4 consecutive 2-game stretches")))

    (testing "3-game stretches"
      (let [stretches (calc/find-consecutive-stretches simple-matchups 3)]
        (is (= 3 (count stretches))
            "5 games should yield 3 consecutive 3-game stretches")))

    (testing "4-game stretches"
      (let [stretches (calc/find-consecutive-stretches simple-matchups 4)]
        (is (= 2 (count stretches))
            "5 games should yield 2 consecutive 4-game stretches")))

    (testing "5-game stretches"
      (let [stretches (calc/find-consecutive-stretches simple-matchups 5)]
        (is (= 1 (count stretches))
            "5 games should yield 1 consecutive 5-game stretch")))

    (testing "Not enough games"
      (let [stretches (calc/find-consecutive-stretches simple-matchups 6)]
        (is (nil? stretches)
            "Should return nil when stretch-length exceeds matchups")))))

(deftest test-find-consecutive-stretches-calculations
  (testing "find-consecutive-stretches calculates totals and averages correctly"
    (let [stretches (calc/find-consecutive-stretches simple-matchups 2)
          first-stretch (first stretches)]

      (testing "First 2-game stretch (weeks 1-2)"
        (is (= 0 (:start-index first-stretch)))
        (is (= 1 (:end-index first-stretch)))
        (is (= 210.0 (:total-for first-stretch))
            "100 + 110 = 210")
        (is (= 105.0 (:avg-for first-stretch))
            "210 / 2 = 105")
        (is (= 185.0 (:total-against first-stretch))
            "90 + 95 = 185")
        (is (= 92.5 (:avg-against first-stretch))
            "185 / 2 = 92.5"))

      (testing "Stretch contains correct weeks"
        (is (= 2 (count (:stretch first-stretch))))
        (is (= [1 2] (map :week (:stretch first-stretch))))))))

(deftest test-find-consecutive-stretches-with-deviations
  (testing "find-consecutive-stretches with deviation fields"
    (let [matchups-with-dev [{:week 1 :points-for 100.0 :points-against 90.0
                              :points-for-deviation 10.0 :points-against-deviation 5.0}
                             {:week 2 :points-for 110.0 :points-against 95.0
                              :points-for-deviation 20.0 :points-against-deviation -5.0}
                             {:week 3 :points-for 90.0 :points-against 100.0
                              :points-for-deviation -10.0 :points-against-deviation 10.0}]
          stretches (calc/find-consecutive-stretches matchups-with-dev 2)
          first-stretch (first stretches)]

      (testing "Deviation totals are calculated"
        (is (= 30.0 (:total-for-deviation first-stretch))
            "10 + 20 = 30")
        (is (= 0.0 (:total-against-deviation first-stretch))
            "5 + (-5) = 0"))

      (testing "Deviation averages are calculated"
        (is (= 15.0 (:avg-for-deviation first-stretch))
            "30 / 2 = 15")
        (is (= 0.0 (:avg-against-deviation first-stretch))
            "0 / 2 = 0")))))

;; ==================== P-VALUE TESTS ====================

(deftest test-p-value-high-direction
  (testing "p-value calculation for :high direction"

    (testing "Value at mean has p-value ~0.5"
      (let [p (calc/p-value {:value 100.0
                             :mean 100.0
                             :std-dev 10.0
                             :direction :high})]
        (is (< 0.49 p 0.51)
            "Value at mean should have p-value around 0.5")))

    (testing "Value 1 std-dev above mean has p-value ~0.16"
      (let [p (calc/p-value {:value 110.0
                             :mean 100.0
                             :std-dev 10.0
                             :direction :high})]
        (is (< 0.15 p 0.17)
            "1 sigma above should have p-value ~0.16 (1 - 0.84)")))

    (testing "Value 2 std-dev above mean has p-value ~0.023"
      (let [p (calc/p-value {:value 120.0
                             :mean 100.0
                             :std-dev 10.0
                             :direction :high})]
        (is (< 0.02 p 0.03)
            "2 sigma above should have p-value ~0.023")))

    (testing "Value 3 std-dev above mean has very low p-value"
      (let [p (calc/p-value {:value 130.0
                             :mean 100.0
                             :std-dev 10.0
                             :direction :high})]
        (is (< p 0.002)
            "3 sigma above should have p-value < 0.002")))

    (testing "Value below mean has high p-value"
      (let [p (calc/p-value {:value 80.0
                             :mean 100.0
                             :std-dev 10.0
                             :direction :high})]
        (is (> p 0.95)
            "Value below mean should have high p-value for :high direction")))))

(deftest test-p-value-low-direction
  (testing "p-value calculation for :low direction"

    (testing "Value at mean has p-value ~0.5"
      (let [p (calc/p-value {:value 100.0
                             :mean 100.0
                             :std-dev 10.0
                             :direction :low})]
        (is (< 0.49 p 0.51))))

    (testing "Value 1 std-dev below mean has p-value ~0.16"
      (let [p (calc/p-value {:value 90.0
                             :mean 100.0
                             :std-dev 10.0
                             :direction :low})]
        (is (< 0.15 p 0.17))))

    (testing "Value 2 std-dev below mean has p-value ~0.023"
      (let [p (calc/p-value {:value 80.0
                             :mean 100.0
                             :std-dev 10.0
                             :direction :low})]
        (is (< 0.02 p 0.03))))

    (testing "Value above mean has high p-value"
      (let [p (calc/p-value {:value 120.0
                             :mean 100.0
                             :std-dev 10.0
                             :direction :low})]
        (is (> p 0.95)
            "Value above mean should have high p-value for :low direction")))))

(deftest test-p-value-both-direction
  (testing "p-value calculation for :both direction (two-tailed)"

    (testing "Value at mean has p-value ~1.0"
      (let [p (calc/p-value {:value 100.0
                             :mean 100.0
                             :std-dev 10.0
                             :direction :both})]
        (is (< 0.99 p 1.01))))

    (testing "Value 2 std-dev from mean (either direction) has p-value ~0.046"
      (let [p-high (calc/p-value {:value 120.0
                                  :mean 100.0
                                  :std-dev 10.0
                                  :direction :both})
            p-low (calc/p-value {:value 80.0
                                 :mean 100.0
                                 :std-dev 10.0
                                 :direction :both})]
        (is (< 0.04 p-high 0.05)
            "2 sigma above should have p-value ~0.046 for two-tailed")
        (is (< 0.04 p-low 0.05)
            "2 sigma below should have p-value ~0.046 for two-tailed")
        (is (< (Math/abs (- p-high p-low)) 0.001)
            "Two-tailed p-values should be symmetric")))))

(deftest test-p-value-extreme-cases
  (testing "p-value extreme cases"

    (testing "Very extreme positive value"
      (let [p (calc/p-value {:value 150.0
                             :mean 100.0
                             :std-dev 10.0
                             :direction :high})]
        (is (< p 0.00001)
            "5 sigma above should have extremely low p-value")))

    (testing "Very extreme negative value"
      (let [p (calc/p-value {:value 50.0
                             :mean 100.0
                             :std-dev 10.0
                             :direction :low})]
        (is (< p 0.00001)
            "5 sigma below should have extremely low p-value")))))

;; ==================== ANOMALOUS STRETCHES TESTS ====================

(deftest test-anomalous-stretches-hot-streak
  (testing "anomalous-stretches detects hot scoring streaks"
    (let [matchups [{:week 1 :points-for 100.0 :points-against 90.0
                     :points-for-deviation 0.0 :points-against-deviation 0.0}
                    {:week 2 :points-for 140.0 :points-against 95.0
                     :points-for-deviation 40.0 :points-against-deviation 5.0}
                    {:week 3 :points-for 150.0 :points-against 85.0
                     :points-for-deviation 50.0 :points-against-deviation -5.0}
                    {:week 4 :points-for 145.0 :points-against 90.0
                     :points-for-deviation 45.0 :points-against-deviation 0.0}]
          ;; Using low std-dev to ensure detection
          anomalies (calc/anomalous-stretches {:username "Alice"
                                               :matchups matchups
                                               :league-wide-std-dev 10.0
                                               :team-mean 100.0
                                               :p-threshold 0.05})]

      (testing "Hot streak is detected"
        (is (some #(= :hot-streak (:type %)) anomalies)
            "Should detect at least one hot streak"))

      (testing "Hot streak has correct structure"
        (let [hot-streak (first (filter #(= :hot-streak (:type %)) anomalies))]
          (is (= "Alice" (:username hot-streak)))
          (is (contains? hot-streak :weeks))
          (is (contains? hot-streak :total))
          (is (contains? hot-streak :average))
          (is (contains? hot-streak :p-value))
          (is (contains? hot-streak :z-score))
          (is (pos? (:z-score hot-streak))
              "Hot streak should have positive z-score")
          (is (< (:p-value hot-streak) 0.05)
              "Anomaly p-value should be below threshold"))))))

(deftest test-anomalous-stretches-cold-streak
  (testing "anomalous-stretches detects cold scoring streaks"
    (let [matchups [{:week 1 :points-for 100.0 :points-against 90.0
                     :points-for-deviation 0.0 :points-against-deviation 0.0}
                    {:week 2 :points-for 60.0 :points-against 95.0
                     :points-for-deviation -40.0 :points-against-deviation 5.0}
                    {:week 3 :points-for 55.0 :points-against 85.0
                     :points-for-deviation -45.0 :points-against-deviation -5.0}
                    {:week 4 :points-for 50.0 :points-against 90.0
                     :points-for-deviation -50.0 :points-against-deviation 0.0}]
          anomalies (calc/anomalous-stretches {:username "Bob"
                                               :matchups matchups
                                               :league-wide-std-dev 10.0
                                               :team-mean 100.0
                                               :p-threshold 0.05})]

      (testing "Cold streak is detected"
        (is (some #(= :cold-streak (:type %)) anomalies)
            "Should detect at least one cold streak"))

      (testing "Cold streak has correct structure"
        (let [cold-streak (first (filter #(= :cold-streak (:type %)) anomalies))]
          (is (= "Bob" (:username cold-streak)))
          (is (neg? (:z-score cold-streak))
              "Cold streak should have negative z-score")
          (is (< (:p-value cold-streak) 0.05)))))))

(deftest test-anomalous-stretches-opponent-strength
  (testing "anomalous-stretches detects opponent strength anomalies"
    (let [matchups [{:week 1 :points-for 100.0 :points-against 140.0
                     :points-for-deviation 0.0 :points-against-deviation 40.0}
                    {:week 2 :points-for 95.0 :points-against 145.0
                     :points-for-deviation -5.0 :points-against-deviation 45.0}
                    {:week 3 :points-for 105.0 :points-against 150.0
                     :points-for-deviation 5.0 :points-against-deviation 50.0}]
          anomalies (calc/anomalous-stretches {:username "Charlie"
                                               :matchups matchups
                                               :league-wide-std-dev 10.0
                                               :team-mean 100.0
                                               :p-threshold 0.05})]

      (testing "Opponent hot streak is detected"
        (is (some #(= :opponent-hot-streak (:type %)) anomalies)
            "Should detect unusually strong opponents"))

      (testing "Opponent hot streak has correct fields"
        (let [opp-hot (first (filter #(= :opponent-hot-streak (:type %)) anomalies))]
          (is (contains? opp-hot :total-against))
          (is (contains? opp-hot :average-against))
          (is (contains? opp-hot :opponent-overperformance))
          (is (pos? (:z-score opp-hot))
              "Opponent overperformance should have positive z-score"))))))

(deftest test-anomalous-stretches-multiple-lengths
  (testing "anomalous-stretches checks multiple stretch lengths"
    (let [matchups (vec (for [i (range 1 7)]
                          {:week i
                           :points-for (+ 100.0 (* i 10))
                           :points-against 95.0
                           :points-for-deviation (* i 10)
                           :points-against-deviation 0.0}))
          anomalies (calc/anomalous-stretches {:username "Dave"
                                               :matchups matchups
                                               :league-wide-std-dev 10.0
                                               :team-mean 100.0
                                               :p-threshold 0.10})]

      (testing "Checks multiple stretch lengths (2-5 games)"
        (let [stretch-lengths (set (map :stretch-length anomalies))]
          (is (some #{2 3 4 5} stretch-lengths)
              "Should find anomalies of various lengths"))))))

(deftest test-anomalous-stretches-threshold-respect
  (testing "anomalous-stretches respects p-value threshold"
    (let [matchups [{:week 1 :points-for 105.0 :points-against 95.0
                     :points-for-deviation 5.0 :points-against-deviation 0.0}
                    {:week 2 :points-for 110.0 :points-against 90.0
                     :points-for-deviation 10.0 :points-against-deviation 0.0}]
          strict-anomalies (calc/anomalous-stretches {:username "Eve"
                                                      :matchups matchups
                                                      :league-wide-std-dev 10.0
                                                      :team-mean 100.0
                                                      :p-threshold 0.001})
          lenient-anomalies (calc/anomalous-stretches {:username "Eve"
                                                       :matchups matchups
                                                       :league-wide-std-dev 10.0
                                                       :team-mean 100.0
                                                       :p-threshold 0.50})]

      (testing "Strict threshold finds fewer anomalies"
        (is (<= (count strict-anomalies) (count lenient-anomalies))
            "Stricter threshold should find same or fewer anomalies"))

      (testing "All anomalies meet their threshold"
        (doseq [anomaly strict-anomalies]
          (is (<= (:p-value anomaly) 0.001)
              "All strict anomalies should have p-value <= 0.001"))
        (doseq [anomaly lenient-anomalies]
          (is (<= (:p-value anomaly) 0.50)
              "All lenient anomalies should have p-value <= 0.50"))))))

(deftest test-anomalous-stretches-z-score-calculation
  (testing "anomalous-stretches calculates z-scores correctly"
    (let [matchups [{:week 1 :points-for 130.0 :points-against 90.0
                     :points-for-deviation 30.0 :points-against-deviation 0.0}
                    {:week 2 :points-for 120.0 :points-against 95.0
                     :points-for-deviation 20.0 :points-against-deviation 5.0}]
          std-dev 10.0
          anomalies (calc/anomalous-stretches {:username "Frank"
                                               :matchups matchups
                                               :league-wide-std-dev std-dev
                                               :team-mean 100.0
                                               :p-threshold 0.20})]

      (testing "Z-score calculation matches expected formula"
        (when-let [hot-streak (first (filter #(= :hot-streak (:type %)) anomalies))]
          ;; For 2-game stretch: total deviation = 50.0
          ;; std-dev for sum = 10.0 * sqrt(2) ≈ 14.14
          ;; z-score = 50.0 / 14.14 ≈ 3.54
          (let [expected-z-score (/ 50.0 (* std-dev (Math/sqrt 2)))]
            (is (< (Math/abs (- (:z-score hot-streak) expected-z-score)) 0.01)
                (str "Z-score should be approximately " expected-z-score))))))))

(deftest test-anomalous-stretches-empty-matchups
  (testing "anomalous-stretches handles edge cases"

    (testing "Empty matchups"
      (let [anomalies (calc/anomalous-stretches {:username "Ghost"
                                                  :matchups []
                                                  :league-wide-std-dev 10.0
                                                  :team-mean 100.0
                                                  :p-threshold 0.05})]
        (is (empty? anomalies)
            "Should return empty vector for empty matchups")))

    (testing "Single matchup"
      (let [anomalies (calc/anomalous-stretches {:username "Loner"
                                                  :matchups [{:week 1 :points-for 100.0
                                                              :points-for-deviation 0.0}]
                                                  :league-wide-std-dev 10.0
                                                  :team-mean 100.0
                                                  :p-threshold 0.05})]
        (is (empty? anomalies)
            "Should return empty vector for single matchup (min stretch is 2)")))))

(deftest test-anomalous-stretches-week-tracking
  (testing "anomalous-stretches tracks weeks correctly"
    (let [matchups [{:week 3 :points-for 140.0 :points-against 90.0
                      :points-for-deviation 40.0 :points-against-deviation 0.0}
                    {:week 4 :points-for 145.0 :points-against 95.0
                      :points-for-deviation 45.0 :points-against-deviation 5.0}
                    {:week 5 :points-for 150.0 :points-against 85.0
                      :points-for-deviation 50.0 :points-against-deviation -5.0}]
          anomalies (calc/anomalous-stretches {:username "Grace"
                                                :matchups matchups
                                                :league-wide-std-dev 10.0
                                                :team-mean 100.0
                                                :p-threshold 0.10})]

      (testing "Weeks are correctly recorded"
        (when-let [anomaly (first anomalies)]
          (is (= (:start-week anomaly) (first (:weeks anomaly)))
              "Start week should match first week in weeks vector")
          (is (= (:end-week anomaly) (last (:weeks anomaly)))
              "End week should match last week in weeks vector")
          (is (= (:stretch-length anomaly) (count (:weeks anomaly)))
              "Stretch length should equal number of weeks"))))))

(deftest test-full-calculation-pipeline
  (testing "Complete calculation pipeline with realistic data"
    (let [;; Set up a mini-season with 4 teams and 3 weeks
          matchups-by-week [{:week 1 :season "2024"
                              :matchups [{:username "Team A" :points-for 120.0}
                                        {:username "Team B" :points-for 100.0}
                                        {:username "Team C" :points-for 90.0}
                                        {:username "Team D" :points-for 110.0}]}
                            {:week 2 :season "2024"
                              :matchups [{:username "Team A" :points-for 125.0}
                                        {:username "Team B" :points-for 95.0}
                                        {:username "Team C" :points-for 105.0}
                                        {:username "Team D" :points-for 115.0}]}
                            {:week 3 :season "2024"
                              :matchups [{:username "Team A" :points-for 130.0}
                                        {:username "Team B" :points-for 98.0}
                                        {:username "Team C" :points-for 88.0}
                                        {:username "Team D" :points-for 112.0}]}]
          team-stats (calc/team-season-stats matchups-by-week)]

      (testing "Team stats calculated correctly"
        (is (= 4 (count team-stats)))
        (is (every? #(contains? team-stats %) ["Team A" "Team B" "Team C" "Team D"])))

      (testing "Team A has consistently high scores"
        (let [team-a-stats (get team-stats "Team A")]
          (is (= 125.0 (:mean team-a-stats))
              "Team A average: (120 + 125 + 130) / 3 = 125")))

      (testing "Stats are reasonable"
        (doseq [[team stats] team-stats]
          (is (pos? (:mean stats))
              (str team " should have positive mean"))
          (is (>= (:std-dev stats) 0)
              (str team " should have non-negative std-dev")))))))
