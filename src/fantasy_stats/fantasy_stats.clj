(ns fantasy-stats.fantasy-stats
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [clojure.edn :as edn])
  (:import (org.apache.commons.math3.stat.descriptive SummaryStatistics))
  (:gen-class))

(defn read-data []
  (let [all-files (.list (io/file "resources/"))]
    (mapv #(edn/read-string (slurp (io/resource %))) all-files)))

;; (defn calculate-points-against-streaks [season-data]
;;   (let [{:keys [season roster-id->username matchups]} season-data]
;;     ))

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

(defn calculate-mean [numbers]
  (let [stats (SummaryStatistics.)]
    (doseq [n numbers]
      (.addValue stats (double n)))
    (.getMean stats)))

(defn -main
  [& args]
  (doseq [season-data (read-data)]
    (let [{matchups-by-week :matchups roster-id->username :roster-id->username} season-data
          parsed-matchups-by-week (reduce
                                   (fn [acc {:keys [season week matchups]}]
                                     (conj acc {:week week
                                                :season season
                                                :matchups (parse-matchups matchups roster-id->username)}))
                                   []
                                   matchups-by-week)
          season-average-points (calculate-mean (mapcat (fn [{:keys [matchups]}]
                                                          (map :points-for matchups))
                                                        parsed-matchups-by-week))
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
                                league-members)]
      (println (format "season: %s, average points: %s" (:season season-data) season-average-points))
      (pp/pprint (first matchups-by-username)))))
