(set-env!
 :source-paths #{"src"}
 :dependencies '[[org.clojure/clojure "1.6.0"] 
                 [com.stuartsierra/component "0.2.1"]   
                 [enlive "1.1.5"]           
                 [incanter "1.5.5"]
                 [org.clojure/data.json "0.2.5"]])

(task-options!
 repl {:server true
       :port 44444
       :init-ns 'user})

(deftask develop
  "Setup for development"
  []
  (set-env! :source-paths #(conj % "dev"))
  (set-env! :dependencies
            #(vec
              (concat %
                      '[[org.clojure/tools.namespace "0.2.3"]
                        [org.clojure/java.classpath "0.2.0"]])))
  identity)

(deftask dev-repl
  "Start repl with extended develop classpath"
  []
  (comp (develop)
        (repl)
        (wait)))

