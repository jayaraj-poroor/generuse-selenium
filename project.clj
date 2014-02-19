; Copyright (c) Jayaraj Poroor. All rights reserved.
; The use and distribution terms for this software are covered by the
; GNU Lesser General Public License 3.0 
; (http://www.gnu.org/copyleft/lesser.html)
; which can be found in the file agpl-3.0.html at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(defproject org.generuse/generuse-selenium "0.1.4-SNAPSHOT"
  :description "Generuse selenium module"
  :url "http://generuse.org"
  :license {:name "GNU Lesser General Public License"
            :url "http://www.gnu.org/copyleft/lesser.html"}
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
