(defproject reacl-ext "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  
  :dependencies [[org.clojure/clojure "1.10.0"] ;; TODO: minimal version?
                 [org.clojure/clojurescript "1.10.520"]
                 [active-clojure "0.26.0"]
                 [reacl "2.1.0"]]

  :profiles {:dev {:dependencies [[midje "1.9.6"]]
                   :plugins [[lein-midje "3.2.1"]]}}

  :plugins [[lein-cljsbuild "1.1.7"]]

  :cljsbuild
  {:builds [{:id "todo"
             :source-paths ["src" "examples/todo"]
             :compiler {:output-to "target/todo/main.js"
                        :output-dir "target/todo/out"
                        :asset-path "../../target/todo"

                        ;;:source-map "target/todo/main.map"
                        ;;:optimizations :whitespace
                        :source-map true
                        :optimizations :none
                        }}]}
  )
