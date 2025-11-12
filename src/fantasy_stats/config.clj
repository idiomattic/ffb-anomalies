(ns fantasy-stats.config
  (:require [clojure.java.io :as io]
            [aero.core :as aero]))

(def store
  (-> (io/resource "config.edn")
      (aero/read-config)))
