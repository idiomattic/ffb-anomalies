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
  [matchups week roster-id->username]
  (-> (group-by :matchup_id matchups)
      (update-vals (fn [[roster-a roster-b]]
                     [{:username (get roster-id->username (:roster_id roster-a))
                       :week week
                       :points-for (:points roster-a)
                       :points-against (:points roster-b)}
                      {:username (get roster-id->username (:roster_id roster-b))
                       :week week
                       :points-for (:points roster-b)
                       :points-against (:points roster-a)}]))
      (vals)))

(defn -main
  [& args]
  (doseq [season-data (read-data)]
    (let [{matchups-by-week :matchups roster-id->username :roster-id->username} season-data]
      (doseq [week-data matchups-by-week]
        (let [{:keys [season week matchups]} week-data]
          (pp/pprint (parse-matchups matchups week roster-id->username)))))))
