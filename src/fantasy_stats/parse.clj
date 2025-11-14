(ns fantasy-stats.parse
  (:require [clojure.set :as set]
            [fantasy-stats.calculate :as calculate]
            [fantasy-stats.parse :as parse]))

(defn roster-id-to-username-map
  [{:keys [rosters users]}]
  (reduce (fn [acc {:keys [roster_id owner_id]}]
            (assoc acc roster_id (some #(when (= owner_id (:user_id %))
                                          (:display_name %))
                                       users)))
          {}
          rosters))

(defn matchups
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

(defn- remove-sub-stretches
  "Remove anomalous stretches that are fully contained within other stretches
   of the same type for the same user."
  [anomalies]
  (let [grouped (group-by (juxt :username :type) anomalies)]
    (mapcat (fn [group-anomalies]
              (let [sorted (sort-by :p-value group-anomalies)]
                (reduce (fn [kept anomaly]
                          (if (some (fn [other]
                                      (and (not= anomaly other)
                                           (<= (:p-value other) (:p-value anomaly))
                                           (set/subset? (set (:weeks anomaly))
                                                        (set (:weeks other)))))
                                    sorted)
                            kept
                            (conj kept anomaly)))
                        []
                        sorted)))
            (vals grouped))))

(defn calculate-deviation-stats
  "Calculate league-wide statistics on deviations from team averages"
  [parsed-matchups-by-week team-stats]
  (let [all-for-deviations (atom [])
        all-against-deviations (atom [])]

    (doseq [{:keys [matchups]} parsed-matchups-by-week]
      (doseq [{:keys [username opponent points-for points-against]} matchups]
        (when-let [user-mean (get-in team-stats [username :mean])]
          (swap! all-for-deviations conj (- points-for user-mean)))
        (when-let [opponent-mean (get-in team-stats [opponent :mean])]
          (swap! all-against-deviations conj (- points-against opponent-mean)))))

    {:for-std-dev (:std-dev (calculate/summary-stats @all-for-deviations))
     :against-std-dev (:std-dev (calculate/summary-stats @all-against-deviations))}))

(defn anomalies-hybrid
  "Find anomalies using the hybrid approach - team averages with league-wide variance"
  [season-data p-threshold]
  (let [{matchups-by-week :matchups roster-id->username :roster-id->username} season-data

        parsed-matchups-by-week (reduce
                                 (fn [acc {:keys [season week matchups]}]
                                   (conj acc {:week week
                                              :season season
                                              :matchups (parse/matchups matchups roster-id->username)}))
                                 []
                                 matchups-by-week)

        ;; Calculate per-team statistics
        team-stats (calculate/team-season-stats parsed-matchups-by-week)

        ;; Calculate league-wide deviation statistics
        deviation-stats (calculate-deviation-stats parsed-matchups-by-week team-stats)

        ;; Get all league members
        league-members (vals roster-id->username)

        ;; Build matchups by username with relative performance data
        matchups-by-username (reduce
                              (fn [acc username]
                                (assoc acc username
                                       (->> parsed-matchups-by-week
                                            (mapv (fn [{:keys [season week matchups]}]
                                                    (when-let [user-matchup (some #(when (= username (:username %)) %)
                                                                                  matchups)]
                                                      (merge {:season season :week week}
                                                             (dissoc user-matchup :username)))))
                                            (filter :opponent)
                                            vec
                                            (#(calculate/calculate-relative-performances % team-stats))
                                            (map-indexed (fn [i el] (assoc el :matchup-index i))))))
                              {}
                              league-members)

        ;; Find all anomalies using the hybrid approach
        all-anomalies (reduce (fn [acc [username matchups]]
                                (let [team-mean (get-in team-stats [username :mean])]
                                  (if-let [anomalies (seq (calculate/anomalous-stretches-hybrid
                                                           {:username username
                                                            :matchups matchups
                                                            :deviation-stats deviation-stats
                                                            :team-mean team-mean
                                                            :p-threshold p-threshold}))]
                                    (concat acc anomalies)
                                    acc)))
                              []
                              matchups-by-username)

        filtered-anomalies (remove-sub-stretches all-anomalies)]

    {:season-stats {:team-stats team-stats
                    :deviation-stats deviation-stats}
     :anomalies filtered-anomalies}))

;; Keep the old function name for backward compatibility, but use the hybrid approach
(defn anomalies
  [season-data p-threshold]
  (anomalies-hybrid season-data p-threshold))
