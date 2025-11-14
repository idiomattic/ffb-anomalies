(ns fantasy-stats.fantasy-stats-test
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.edn :as edn]
            [clojure.set :as set]
            [fantasy-stats.sleeper :as sleeper]
            [fantasy-stats.parse :as parse]))

(def fixture-data
  (-> (slurp "test/fixtures/2024_league_data.edn")
      edn/read-string
      vector))

(defn with-out-str-and-result
  "Like with-out-str but also returns the function result as [output result]"
  [f]
  (let [s (java.io.StringWriter.)]
    (binding [*out* s]
      (let [result (f)]
        [(str s) result]))))

(defn run-analysis-with-fixtures
  [p-threshold]
  (with-redefs [sleeper/fetch-all-data-memoized (fn [] fixture-data)]
    (with-out-str-and-result
      #(doseq [season-data (sleeper/fetch-all-data-memoized)]
         (let [{:keys [season-stats-for season-stats-against anomalies]}
               (parse/anomalies season-data p-threshold)]
           {:season-data season-data
            :season-stats-for season-stats-for
            :season-stats-against season-stats-against
            :anomalies anomalies})))))

(deftest test-fixture-data-loads
  (testing "Fixture data loads correctly"
    (is (vector? fixture-data))
    (is (= 1 (count fixture-data)))
    (let [season (first fixture-data)]
      (is (= "2024" (:season season)))
      (is (= "1123378242238472192" (:league-id season)))
      (is (= "complete" (:status season)))
      (is (map? (:roster-id->username season)))
      (is (vector? (:matchups season))))))

(deftest test-parse-anomalies-with-fixture-data
  (testing "Parse anomalies function works with fixture data"
    (with-redefs [sleeper/fetch-all-data-memoized (fn [] fixture-data)]
      (let [season-data (first (sleeper/fetch-all-data-memoized))
            result (parse/anomalies season-data 0.01)]

        (testing "Returns expected keys"
          (is (contains? result :season-stats-for))
          (is (contains? result :season-stats-against))
          (is (contains? result :anomalies)))

        (testing "Season stats have correct structure"
          (is (number? (get-in result [:season-stats-for :mean])))
          (is (number? (get-in result [:season-stats-for :std-dev])))
          (is (number? (get-in result [:season-stats-against :mean])))
          (is (number? (get-in result [:season-stats-against :std-dev]))))

        (testing "Season stats are reasonable"
          (let [mean-for (get-in result [:season-stats-for :mean])
                mean-against (get-in result [:season-stats-against :mean])]
            (is (> mean-for 80) "Mean points should be reasonable")
            (is (< mean-for 150) "Mean points should be reasonable")
            (is (> mean-against 80) "Mean points against should be reasonable")
            (is (< mean-against 150) "Mean points against should be reasonable")))

        (testing "Anomalies are returned"
          (is (vector? (:anomalies result))))))))

(deftest test-anomaly-structure
  (testing "Anomalies have correct structure"
    (with-redefs [sleeper/fetch-all-data-memoized (fn [] fixture-data)]
      (let [season-data (first (sleeper/fetch-all-data-memoized))
            result (parse/anomalies season-data 0.05) ; Use higher threshold to get more results
            anomalies (:anomalies result)]

        (when (seq anomalies)
          (let [anomaly (first anomalies)]
            (testing "Anomaly has required keys"
              (is (contains? anomaly :username))
              (is (contains? anomaly :type))
              (is (contains? anomaly :weeks))
              (is (contains? anomaly :total))
              (is (contains? anomaly :average))
              (is (contains? anomaly :p-value))
              (is (contains? anomaly :z-score))
              (is (contains? anomaly :stretch-length)))

            (testing "Anomaly type is valid"
              (is (#{:points-for-high :points-for-low
                     :points-against-high :points-against-low}
                   (:type anomaly))))

            (testing "Anomaly values are reasonable"
              (is (string? (:username anomaly)))
              (is (vector? (:weeks anomaly)))
              (is (pos? (:stretch-length anomaly)))
              (is (<= (:p-value anomaly) 0.05))
              (is (number? (:z-score anomaly))))))))))

(deftest test-different-p-thresholds
  (testing "Different p-value thresholds return different numbers of anomalies"
    (with-redefs [sleeper/fetch-all-data-memoized (fn [] fixture-data)]
      (let [season-data (first (sleeper/fetch-all-data-memoized))
            result-001 (parse/anomalies season-data 0.01)
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
            (is (<= (:p-value anomaly) 0.05))))))))

(deftest test-roster-id-to-username-mapping
  (testing "Roster ID to username mapping works correctly"
    (let [season-data (first fixture-data)
          roster-id->username (:roster-id->username season-data)]

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

(deftest test-matchup-parsing
  (testing "Matchups are parsed correctly"
    (let [season-data (first fixture-data)
          first-week-matchups (-> season-data :matchups first :matchups)
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

(deftest test-no-anomalies-with-very-strict-threshold
  (testing "Very strict p-value threshold may find no anomalies"
    (with-redefs [sleeper/fetch-all-data-memoized (fn [] fixture-data)]
      (let [season-data (first (sleeper/fetch-all-data-memoized))
            result (parse/anomalies season-data 0.0001)]
        (is (vector? (:anomalies result)))))))

(deftest test-all-users-have-matchups
  (testing "All users in roster mapping have matchup data"
    (let [season-data (first fixture-data)
          roster-id->username (:roster-id->username season-data)
          usernames (set (vals roster-id->username))
          first-week-matchups (-> season-data :matchups first :matchups)
          parsed-matchups (parse/matchups first-week-matchups roster-id->username)
          matchup-usernames (set (map :username parsed-matchups))]

      (testing "All roster usernames appear in matchups"
        (is (= usernames matchup-usernames))))))

(deftest test-end-to-end-with-default-threshold
  (testing "End-to-end analysis with default threshold (0.01)"
    (with-redefs [sleeper/fetch-all-data-memoized (fn [] fixture-data)]
      (let [[output result] (run-analysis-with-fixtures 0.01)]

        (testing "Analysis completes without errors"
          (is (string? output)))

        (testing "Output contains expected information"
          (is (re-find #"Season: 2024" output))
          (is (re-find #"Points For - Mean:" output))
          (is (re-find #"Points Against - Mean:" output)))))))

(deftest test-anomaly-filtering
  (testing "Sub-stretches are filtered out correctly"
    (with-redefs [sleeper/fetch-all-data-memoized (fn [] fixture-data)]
      (let [season-data (first (sleeper/fetch-all-data-memoized))
            result (parse/anomalies season-data 0.10) ; Use lenient threshold
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
                      "Smaller stretch should not be subset of larger with better p-value"))))))))))
