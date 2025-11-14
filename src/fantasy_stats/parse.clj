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


(defn anomalies [season-data p-threshold]
  (let [{matchups-by-week :matchups roster-id->username :roster-id->username} season-data
        parsed-matchups-by-week (reduce
                                 (fn [acc {:keys [season week matchups]}]
                                   (conj acc {:week week
                                              :season season
                                              :matchups (parse/matchups matchups roster-id->username)}))
                                 []
                                 matchups-by-week)
        all-points (mapcat (fn [{:keys [matchups]}]
                             (map :points-for matchups)) ;; using points-for, but points-against would return the same since matchups are zero-sum
                           parsed-matchups-by-week)
        season-stats (calculate/summary-stats all-points)
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
                                (if-let [anomalies (seq (calculate/anomalous-stretches {:username username
                                                                                        :matchups matchups
                                                                                        :all-points all-points
                                                                                        :p-threshold p-threshold}))]
                                  (concat acc anomalies)
                                  acc))
                              []
                              matchups-by-username)
        filtered-anomalies (remove-sub-stretches all-anomalies)]
    {:season-stats season-stats
     :anomalies filtered-anomalies}))
