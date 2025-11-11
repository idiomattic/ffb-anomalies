(ns fantasy-stats.fantasy-stats
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [clojure.edn :as edn])
  (:gen-class))

(defn read-data []
  (let [all-files (.list (io/file "resources/"))]
    (mapv #(edn/read-string (slurp (io/resource %))) all-files)))

(defn calculate-points-against-streaks [season-data]
  (let [{:keys [season roster-id->username matchups]} season-data]
    ))

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

(defn -main
  [& args]
  (doseq [season-data (read-data)]
    (let [{matchups-by-week :matchups roster-id->username :roster-id->username} season-data
          parsed-matchups-by-week (reduce (fn [acc {:keys [season week matchups]}]
                                            (conj acc {:week week
                                                       :season season
                                                       :matchups (parse-matchups matchups roster-id->username)}))
                                          []
                                          matchups-by-week)]
      (pp/pprint (first parsed-matchups-by-week)))))
