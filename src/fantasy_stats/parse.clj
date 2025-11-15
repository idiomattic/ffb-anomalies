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
  (->> anomalies
       (group-by (juxt :username :type))
       vals
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
                           sorted))))
       vec))

(defn calculate-deviation-stats
  "Calculate league-wide statistics on deviations from team averages"
  [team-stats]
  (let [;; Collect individual team standard deviations
        team-std-devs (map (fn [[_ stats]] (:std-dev stats)) team-stats)
        avg-team-std-dev (/ (reduce + team-std-devs) (count team-std-devs))
        deviation-std-dev avg-team-std-dev]
    ;; Minimum of 10 points std-dev
    (max deviation-std-dev 10.0)))

(defn anomalies
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

        team-stats (calculate/team-season-stats parsed-matchups-by-week)

        league-wide-std-dev (calculate-deviation-stats team-stats)

        league-members (vals roster-id->username)

        matchups-by-username (reduce
                              (fn [acc username]
                                (assoc acc username
                                       (->> parsed-matchups-by-week
                                            (mapv (fn [{:keys [season week matchups]}]
                                                    (when-let [user-matchup (some #(when (= username (:username %)) %)
                                                                                  matchups)]
                                                      (merge {:season season :week week}
                                                             user-matchup))))
                                            (filter :opponent)
                                            vec
                                            (#(calculate/calculate-relative-performances % team-stats))
                                            (mapv #(dissoc % :username))
                                            (map-indexed (fn [i el] (assoc el :matchup-index i))))))
                              {}
                              league-members)

        ;; Find all anomalies using the hybrid approach
        all-anomalies (reduce (fn [acc [username matchups]]
                                (let [team-mean (get-in team-stats [username :mean])]
                                  (if-let [anomalies (seq (calculate/anomalous-stretches
                                                           {:username username
                                                            :matchups matchups
                                                            :league-wide-std-dev league-wide-std-dev
                                                            :team-mean team-mean
                                                            :p-threshold p-threshold}))]
                                    (concat acc anomalies)
                                    acc)))
                              []
                              matchups-by-username)

        filtered-anomalies (remove-sub-stretches all-anomalies)]

    {:season-stats {:team-stats team-stats
                    :league-wide-std-dev league-wide-std-dev}
     :anomalies filtered-anomalies}))
