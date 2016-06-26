(defproject cl2048 "0.1.0"
  :description "Clone of 2048 game"
  :url "https://github.com/alexander982/cl2048"

  :dependencies [[org.clojure/clojure "1.8.0"]]
  :main ^:skip-aot cl2048.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
