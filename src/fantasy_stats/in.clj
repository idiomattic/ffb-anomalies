(ns fantasy-stats.in
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]))

(defn data [path]
  {:pre [(string? path)
         (not-empty path)]}
  (let [all-files (.list (io/file path))]
    (mapv #(edn/read-string (slurp (io/resource %))) all-files)))
