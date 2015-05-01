{:user
 {:plugins [[cider/cider-nrepl "0.8.2"]
            [refactor-nrepl "1.0.3"]
            [com.jakemccrary/lein-test-refresh "0.9.0"]]
  :dependencies [[spyscope "0.1.4"]
                  [org.clojure/tools.namespace "0.2.4"]
                  [leiningen #=(leiningen.core.main/leiningen-version)]
                  [io.aviso/pretty "0.1.8"]
                  [im.chit/vinyasa "0.3.4"]
                  [org.clojars.gjahad/debug-repl "0.3.3"]]
  :injections [(require 'spyscope.core)
               (require '[vinyasa.inject :as inject])
               (require 'io.aviso.repl)
               (inject/in ;; the default injected namespace is `.` 
                ;; note that `:refer, :all and :exclude can be used
                [vinyasa.inject :refer [inject [in inject-in]]]  
                [vinyasa.lein :exclude [*project*]]  
                
                ;; imports all functions in vinyasa.pull
                [vinyasa.pull :all]
                
                ;; same as [cemerick.pomegranate 
                ;;           :refer [add-classpath get-classpath resources]]
                [cemerick.pomegranate add-classpath get-classpath resources] 
                
                ;; inject into clojure.core 
                clojure.core
                [vinyasa.reflection .> .? .* .% .%> .& .>ns .>var]
                
               ;; inject into clojure.core with prefix
                clojure.core >
                [clojure.pprint pprint]
                [clojure.java.shell sh]
                [clojure.pprint pprint pp]
                [alex-and-georges.debug-repl debug-repl])]}}
