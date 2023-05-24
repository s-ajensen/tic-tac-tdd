(ns tic-tac-toe.db-spec
  (:require [speclj.core :refer :all]
            [tic-tac-toe.db :refer :all])
  (:import (clojure.lang ExceptionInfo)))

(describe "tic tac toe data repository"
  (it "verifies config filename"
    (should= "db-config.json" config-file))

  (it "verifies existence of config file"
    (slurp config-file)
    (should-not-throw (slurp config-file)))

  (describe "db config"
    (with-stubs)

    (it "reads contents of config file"
      (with-redefs [slurp (stub :mock-slurp {:return "{\"db\": {}}"})]
        (get-db-config)
        (should-have-invoked :mock-slurp {:with [config-file]})))

    (it "throws exception if config file is not valid json"
      (with-redefs [slurp (stub :mock-slurp {:return "{\"not\": \"proper json}"})]
        (should-throw ExceptionInfo (str config-file " must be valid JSON!") (get-db-config))))

    (it "contains connection fields"
      (let [config  (get-db-config)]
        (should-contain :classname config)
        (should-contain :subprotocol config)
        (should-contain :subname config)))))