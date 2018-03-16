(require '[boot.core]
         '[boot.repl])


(swap! boot.repl/*default-dependencies*
       concat '[[cider/cider-nrepl "0.16.0"]])

(swap! boot.repl/*default-middleware*
       conj 'cider.nrepl/cider-middleware)

(set-env!
 :source-paths #{"src" "dev"}
 :dependencies '[[org.clojure/clojure "1.9.0-beta2"]
                 [com.stuartsierra/component "0.2.1"]
                 [enlive "1.1.5"]
                 [incanter "1.5.7"]
                 [org.clojure/data.json "0.2.5"]
                 ;;[svm-clj "0.1.3"]


                 [org.clojure/tools.namespace "0.2.3"]
                 [org.clojure/java.classpath "0.2.0"]])
                 

(task-options!
 repl {:server true
       :port 44445
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


