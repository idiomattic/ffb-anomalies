(ns fantasy-stats.download-matchup-data
  (:require [hato.client :as hc]
            [cheshire.core :as json]))

(def current-league-id "1257434174667620353")

(def max-week 17)

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

(defn get-league-rosters [{:keys [league-id]}]
  (let [{:keys [body status]} (hc/request {:method :get
                                           :url (format "https://api.sleeper.app/v1/league/%s/rosters" league-id)
                                           :http-client sleeper-client})]
    (when (not= 200 status)
      (println "error fetching rosters: " body))
    (when (= 200 status)
      (json/parse-string body true))))

(defn get-league-users [{:keys [league-id]}]
  (let [{:keys [body status]} (hc/request {:method :get
                                           :url (format "https://api.sleeper.app/v1/league/%s/users" league-id)
                                           :http-client sleeper-client})]
    (when (not= 200 status)
      (println "error fetching users: " body))
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

(defn make-roster-id-to-username-map
  [{:keys [league-id]}]
  (let [rosters (get-league-rosters {:league-id league-id})
        users (get-league-users {:league-id league-id})]
    (reduce (fn [acc {:keys [roster_id owner_id]}]
              (assoc acc roster_id (some #(when (= owner_id (:user_id %))
                                            (:display_name %))
                                         users)))
            {}
            rosters)))

(defn get-leagues [starting-league-id]
  (loop [league-id starting-league-id
         result []]
    (let [{:keys [previous_league_id status season]} (get-league-info {:league-id league-id})
          league-data {:league-id league-id
                       :status status
                       :season season}]
      (if previous_league_id
        (recur previous_league_id (conj result league-data))
        (conj result league-data)))))

(defn get-league-matchups [{:keys [league-id status season]}]
  (let [weeks-to-fetch (if (= status "complete")
                         (range 1 (inc max-week))
                         (range 1 (inc (get-nfl-state))))]
    (reduce (fn [acc week-number]
              (let [matchups (get-matchups {:league-id league-id :week week-number})]
                (conj acc {:league-id league-id
                           :season season
                           :week week-number
                           :matchups (mapv #(select-keys % [:points :roster_id :matchup_id])
                                           matchups)})))
            []
            weeks-to-fetch)))

(defn -main [& _args]
  (let [leagues (get-leagues current-league-id)]
    (doseq [league leagues]
      (let [{:keys [league-id season]} league
            roster-id->username (make-roster-id-to-username-map {:league-id league-id})
            matchups (get-league-matchups league)]
        (spit (format "resources/%s_league_data.edn" season)
              (merge league
                     {:matchups matchups
                      :roster-id->username roster-id->username}))))))
