(ns fantasy-stats.download-matchup-data
  (:require [hato.client :as hc]
            [cheshire.core :as json]))

(def current-league-id "1257434174667620353")

(def sleeper-client
  (hc/build-http-client {:connect-timeout 10000}))

(defn get-matchups [{:keys [week league-id]}]
  (let [{:keys [body status]} (hc/request {:method :get
                                           :url (format "https://api.sleeper.app/v1/league/%s/matchups/%s" league-id week)
                                           :http-client sleeper-client})]
    (when (not= 200 status)
      (println "error fetching matchups: " body))
    (when (= 200 status)
      (json/parse-string body true))))

(defn get-league-info [{:keys [league-id]}]
  (let [{:keys [body status]} (hc/request {:method :get
                                           :url (format "https://api.sleeper.app/v1/league/%s" league-id)
                                           :http-client sleeper-client})]
    (when (not= 200 status)
      (println "error fetching league: " body))
    (when (= 200 status)
      (json/parse-string body true))))

(defn get-nfl-state
  "Returns a number representing the current NFL week"
  []
  (let [{:keys [body status]} (hc/request {:method :get
                                           :url "https://api.sleeper.app/v1/state/nfl"
                                           :http-client sleeper-client})]
    (when (not= 200 status)
      (println "error fetching matchups: " body))
    (when (= 200 status)
      (:week (json/parse-string body true)))))

(defn get-leagues []
  (loop [league-id current-league-id
         result []]
    (let [{:keys [previous_league_id status season]} (get-league-info {:league-id league-id})
          league-data {:league-id league-id
                       :status status
                       :season season}]
      (if previous_league_id
        (recur previous_league_id (conj result league-data))
        (conj result league-data)))))

(defn -main [& _args]
  (let [current-nfl-week (get-nfl-state)
        leagues (get-leagues)]
    (println "leagues: " leagues)))
