(defproject org.generuse/generuse-selenium "0.1.3-SNAPSHOT"
  :description "Generuse selenium module"
  :url "http://generuse.org"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.seleniumhq.selenium/selenium-java "2.39.0"]
                 [com.opera/operadriver "1.5"]
                 [org.generuse/generuse-lib "0.1.0-SNAPSHOT"]
   				]
  :main generuse.mod.selenium
  :aot  [generuse.mod.selenium]
  :repositories {"sonatype-oss-public"
               "https://oss.sonatype.org/content/groups/public/"}  

  )
