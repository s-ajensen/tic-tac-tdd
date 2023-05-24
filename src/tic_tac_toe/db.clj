(ns tic-tac-toe.db
  (:require [clojure.data.json :as json])
  (:import (java.io EOFException)))

(def config-file "db-config.json")

(defn get-db-config []
  (try
    (json/read-str (slurp config-file) :key-fn keyword)
    (catch EOFException e
      (throw (ex-info (str config-file " must be valid JSON!") {} e)))))

(defn open-game [])