(ns generuse.mod.selenium
	(:gen-class)
   	(:use [generuse.lib.exec :only (deref-eval)])	
    (:import (org.openqa.selenium.firefox FirefoxDriver)
   			 (org.openqa.selenium By)   			
   	)
)

(def _open-browser {:name "open" :target-type :web_browser})
(defn ^{:axon _open-browser} open-browser[target-eval param-evals ctx globals & more]
	(let [driver (FirefoxDriver.)]
		true
	)
)
