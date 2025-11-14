(ns fantasy-stats.main
  (:require [fantasy-stats.config :as config]
            [fantasy-stats.sleeper :as sleeper]
            [fantasy-stats.parse :as parse]
            [fantasy-stats.out :as out])
  (:gen-class))


(defn -main
  [& args]
  (let [p-threshold (if (empty? args)
                      (get-in config/store [:settings :default-p-threshold])
                      (let [parsed (parse-double (first args))]
                        (if (nil? parsed)
                          (do
                            (println "Error: Invalid p-value threshold. Please provide a valid decimal number.")
                            (System/exit 1))
                          parsed)))]

    (out/p-threshold! p-threshold)
    (println)

    (doseq [season-data (sleeper/fetch-all-data-memoized)]
      (let [{:keys [season-stats anomalies]} (parse/anomalies season-data p-threshold)]
        (out/season-info! {:season-data season-data
                           :stats season-stats})
        (println)
        (out/anomalies! anomalies)))))
