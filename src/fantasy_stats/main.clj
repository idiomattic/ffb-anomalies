(ns fantasy-stats.main
  (:require [fantasy-stats.sleeper :as sleeper]
            [fantasy-stats.parse :as parse]
            [fantasy-stats.out :as out])
  (:gen-class))


(defn -main
  [& args]
  (let [p-threshold (if (empty? args)
                      0.01
                      (let [parsed (parse-double (first args))]
                        (if (nil? parsed)
                          (do
                            (println "Error: Invalid p-value threshold. Please provide a valid decimal number.")
                            (System/exit 1))
                          parsed)))]

    (out/p-threshold! p-threshold)
    (println)

    (doseq [season-data (sleeper/fetch-all-data-memoized)]
      (let [{:keys [season-stats-for season-stats-against anomalies]} (parse/anomalies season-data p-threshold)]
        (out/season-info! {:season-data season-data
                           :stats-for season-stats-for
                           :stats-against season-stats-against})
        (println)
        (out/anomalies! anomalies)))))
