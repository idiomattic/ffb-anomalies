(ns fantasy-stats.parse-test
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.edn :as edn]
            [clojure.set :as set]
            [fantasy-stats.parse :as parse]))

(def season-data
  "Load test fixture data from EDN file"
  (-> (slurp "test/fixtures/2024_league_data.edn")
      edn/read-string))

;; ==================== DATA STRUCTURE TESTS ====================

(deftest test-fixture-data-loads
  (testing "Fixture data loads correctly"
    (is (map? season-data))
    (is (= "2024" (:season season-data)))
    (is (= "1123378242238472192" (:league-id season-data)))
    (is (= "complete" (:status season-data)))
    (is (map? (:roster-id->username season-data)))
    (is (vector? (:matchups season-data)))))

(deftest test-roster-id-to-username-mapping
  (testing "Roster ID to username mapping works correctly"
    (let [roster-id->username (:roster-id->username season-data)]

      (testing "Mapping exists and has correct size"
        (is (map? roster-id->username))
        (is (= 12 (count roster-id->username))))

      (testing "All roster IDs from 1-12 are mapped"
        (doseq [roster-id (range 1 13)]
          (is (contains? roster-id->username roster-id)
              (str "Roster ID " roster-id " should be in mapping"))))

      (testing "Sample usernames are correct"
        (is (= "Eric" (get roster-id->username 1)))
        (is (= "Austin" (get roster-id->username 7)))
        (is (= "Gabe" (get roster-id->username 12)))))))

;; ==================== MATCHUP PARSING TESTS ====================

(deftest test-matchup-parsing
  (testing "Matchups are parsed correctly"
    (let [first-week-matchups (-> season-data :matchups first :matchups)
          roster-id->username (:roster-id->username season-data)
          parsed (parse/matchups first-week-matchups roster-id->username)]

      (testing "Correct number of matchup records"
        (is (= 12 (count parsed)) "Should have 12 records (one per team)"))

      (testing "Each matchup has required keys"
        (doseq [matchup parsed]
          (is (contains? matchup :username))
          (is (contains? matchup :opponent))
          (is (contains? matchup :points-for))
          (is (contains? matchup :points-against))))

      (testing "Points are symmetric"
        (let [by-username (group-by :username parsed)]
          (doseq [[username [matchup]] by-username]
            (let [opponent-name (:opponent matchup)
                  opponent-matchup (first (get by-username opponent-name))]
              (is (= (:points-for matchup) (:points-against opponent-matchup))
                  (str username "'s points-for should equal " opponent-name "'s points-against"))
              (is (= (:points-against matchup) (:points-for opponent-matchup))
                  (str username "'s points-against should equal " opponent-name "'s points-for")))))))))

(deftest test-all-users-have-matchups
  (testing "All users in roster mapping have matchup data"
    (let [roster-id->username (:roster-id->username season-data)
          usernames (set (vals roster-id->username))
          first-week-matchups (-> season-data :matchups first :matchups)
          parsed-matchups (parse/matchups first-week-matchups roster-id->username)
          matchup-usernames (set (map :username parsed-matchups))]

      (testing "All roster usernames appear in matchups"
        (is (= usernames matchup-usernames))))))

;; ==================== ANOMALY DETECTION TESTS ====================

(deftest test-parse-anomalies-with-fixture-data
  (testing "Parse anomalies function works with fixture data"
    (let [result (parse/anomalies season-data 0.01)]

      (testing "Returns expected keys"
        (is (contains? result :season-stats))
        (is (contains? result :anomalies)))

      (testing "Season stats have correct structure"
        (let [season-stats (:season-stats result)]
          (is (contains? season-stats :team-stats))
          (is (contains? season-stats :league-wide-std-dev))
          (is (map? (:team-stats season-stats)))
          (is (number? (:league-wide-std-dev season-stats)))))

      (testing "Team stats are reasonable"
        (let [team-stats (get-in result [:season-stats :team-stats])]
          (doseq [[username stats] team-stats]
            (is (number? (:mean stats)) (str username " should have numeric mean"))
            (is (number? (:std-dev stats)) (str username " should have numeric std-dev"))
            (is (> (:mean stats) 60) (str username " mean should be reasonable (>60)"))
            (is (< (:mean stats) 200) (str username " mean should be reasonable (<200)")))))

      (testing "Anomalies are returned"
        (is (vector? (:anomalies result)))))))

(deftest test-anomaly-structure
  (testing "Anomalies have correct structure"
    (let [result (parse/anomalies season-data 0.05) ; Use higher threshold to get more results
          anomalies (:anomalies result)]

      (when (seq anomalies)
        (let [anomaly (first anomalies)]
          (testing "Anomaly has required keys"
            (is (contains? anomaly :username))
            (is (contains? anomaly :type))
            (is (contains? anomaly :weeks))
            (is (contains? anomaly :p-value))
            (is (contains? anomaly :z-score))
            (is (contains? anomaly :stretch-length)))

          (testing "Anomaly type is valid"
            (is (#{:hot-streak :cold-streak
                   :opponent-hot-streak :opponent-cold-streak}
                 (:type anomaly))))

          (testing "Anomaly values are reasonable"
            (is (string? (:username anomaly)))
            (is (vector? (:weeks anomaly)))
            (is (pos? (:stretch-length anomaly)))
            (is (<= (:p-value anomaly) 0.05))
            (is (number? (:z-score anomaly)))))))))

(deftest test-different-p-thresholds
  (testing "Different p-value thresholds return different numbers of anomalies"
    (let [result-001 (parse/anomalies season-data 0.01)
          result-005 (parse/anomalies season-data 0.05)
          result-010 (parse/anomalies season-data 0.10)]

      (testing "More lenient threshold finds more anomalies"
        (is (>= (count (:anomalies result-010))
                (count (:anomalies result-005))))
        (is (>= (count (:anomalies result-005))
                (count (:anomalies result-001)))))

      (testing "All anomalies meet their threshold"
        (doseq [anomaly (:anomalies result-001)]
          (is (<= (:p-value anomaly) 0.01)))
        (doseq [anomaly (:anomalies result-005)]
          (is (<= (:p-value anomaly) 0.05)))))))

(deftest test-no-anomalies-with-very-strict-threshold
  (testing "Very strict p-value threshold may find no anomalies"
    (let [result (parse/anomalies season-data 0.0001)]
      (is (vector? (:anomalies result))))))

(deftest test-anomaly-filtering
  (testing "Sub-stretches are filtered out correctly"
    (let [result (parse/anomalies season-data 0.10) ; Use lenient threshold
          anomalies (:anomalies result)]

      (when (> (count anomalies) 1)
        (testing "No anomaly is a complete subset of another with better p-value"
          (doseq [anomaly-a anomalies
                  anomaly-b anomalies]
            (when (and (not= anomaly-a anomaly-b)
                       (= (:username anomaly-a) (:username anomaly-b))
                       (= (:type anomaly-a) (:type anomaly-b))
                       (<= (:p-value anomaly-b) (:p-value anomaly-a)))
              (let [weeks-a (set (:weeks anomaly-a))
                    weeks-b (set (:weeks anomaly-b))]
                (is (not (set/subset? weeks-a weeks-b))
                    "Smaller stretch should not be subset of larger with better p-value")))))))))

;; ==================== INTEGRATION TEST ====================

(deftest test-end-to-end-analysis
  (testing "End-to-end analysis with default threshold (0.01)"
    (let [result (parse/anomalies season-data 0.01)]

      (testing "Analysis completes successfully"
        (is (map? result))
        (is (contains? result :season-stats))
        (is (contains? result :anomalies)))

      (testing "Season stats computed"
        (is (map? (get-in result [:season-stats :team-stats])))
        (is (= 12 (count (get-in result [:season-stats :team-stats])))
            "Should have stats for all 12 teams"))

      (testing "Anomalies structure is correct"
        (is (vector? (:anomalies result)))
        (doseq [anomaly (:anomalies result)]
          (is (<= (:p-value anomaly) 0.01)
              "All anomalies should meet the threshold"))))))
