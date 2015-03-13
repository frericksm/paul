(defproject paulkrake "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [com.stuartsierra/component "0.2.1"]
                 [enlive "1.1.5"]
                 [incanter "1.5.5"]
                 [org.clojure/data.json "0.2.5"]
                 [svm-clj "0.1.3"]]
  :profiles {:dev {:source-paths ["dev"]
                   :dependencies [[org.clojure/tools.namespace "0.2.3"]
                                  [org.clojure/java.classpath "0.2.0"]]}}
  :repl-options {:port 4555}
  :plugins [[cider/cider-nrepl "0.8.2"]]
)
