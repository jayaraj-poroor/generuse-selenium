(ns generuse.mod.selenium
	(:gen-class)
   	(:use [generuse.lib.exec :only (deref-eval)])	
    (:import (org.openqa.selenium.firefox FirefoxDriver)
   			 (org.openqa.selenium By)   			
   	)
)

(def open-browser_ {:name "open" :target-type :web_browser})
(defn ^{:axon open-browser_} open-browser[target-eval param-evals 
											ctx globals & more]
	(let [driver (FirefoxDriver.)]
		(dosync 
			(alter globals assoc "address-bar"
				(ref {:type :web_address-bar :value driver})
			)
		)
	)
)


(def input-address-bar_ {:name "input" :target-type :web_address-bar})
(defn ^{:axon input-address-bar_} input-address-bar[target-eval param-evals 
														ctx globals & more]
	(let [target (deref-eval target-eval)
		  driver (:value target)
		  url    (:value (deref-eval (:with param-evals)))
		 ]
		(.get driver url)
	)
)
