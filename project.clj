(defproject cwc-overlord "0.1.0-SNAPSHOT"
  :description "The Checkers with Crowds Overlord"
  :license {:name "The MIT License"
            :url "http://choosealicense.com/licenses/mit/"}
  :aot [cwc.pretend]
  :main cwc.pretend
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [clj-http "0.7.6"]
                 [clj-time "0.6.0"]
                 [cheshire "5.2.0"]])
