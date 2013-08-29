(defproject cwc-overlord "0.1.0-SNAPSHOT"
  :description "The Checkers with Crowds Overlord"
  :license {:name "The MIT License"
            :url "http://choosealicense.com/licenses/mit/"}
  :aot [cwc.pretend cwc.overlord]
  :main cwc.overlord
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/tools.cli "0.2.4"]
                 [org.flatland/useful "0.10.3"]
                 [clojurewerkz/urly "1.0.0"]
                 [overtone/at-at "1.2.0"]
                 [clj-http "0.7.6"]
                 [clj-time "0.6.0"]
                 [cheshire "5.2.0"]])
