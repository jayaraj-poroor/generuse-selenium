; Copyright (c) Jayaraj Poroor. All rights reserved.
; The use and distribution terms for this software are covered by the
; GNU Lesser General Public License 3.0 
; (http://www.gnu.org/copyleft/lesser.html)
; which can be found in the file lgpl-3.0.html at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns generuse.mod.selenium
	(:gen-class)
   	(:use [generuse.lib.exec :only (deref-eval)])	
    (:import (org.openqa.selenium.firefox FirefoxDriver)
   			 (org.openqa.selenium By)   			
   	)
)

(def opens-browser_ {:name "opens" :target-type :web_browser})
(defn ^{:axon opens-browser_} open-browser[target-eval param-evals 
											ctx globals & more]
	(let [driver (FirefoxDriver.)]
		(dosync
		    (alter (:value target-eval) assoc :value driver) 
			(alter globals assoc "address-bar"
				(ref {:type :web_address-bar :value driver})
			)
		)
	)
)

(def shows-browser_ {:name "shows" :target-type :web_browser})
(defn ^{:axon shows-browser_} shows-browser[target-eval param-evals 
											ctx globals & more]
	(let [driver (:value (deref-eval target-eval))]
		(if (.contains (.toString driver) "null")
			{:value false :type :boolean :pass false}
			{:value true :type :boolean :pass true}			
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
