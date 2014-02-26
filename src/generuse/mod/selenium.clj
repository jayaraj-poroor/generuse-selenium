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
   			 (org.openqa.selenium By NoSuchElementException)
   	)
)

(def open-browser_ {:names ["open"] :target-type :web_browser})
(defn ^{:axon open-browser_} open-browser[target-eval param-evals 
											ctx globals & more]
	(let [driver (FirefoxDriver.)]
		(dosync
		    (alter (:value target-eval) assoc :value {:driver driver}) 
		)
	)
)

(def show-browser_ {:names ["show", "is-shown?"] :target-type :web_browser})
(defn ^{:axon show-browser_} show-browser[target-eval param-evals 
											ctx globals & more]
	(let [driver (get-in (deref-eval target-eval) [:value :driver] )]
		(if (and driver (not (.contains (.toString driver) "null")))
			{:value true :type :boolean :pass true}
			{:value false :type :boolean :pass false}			
		)
	)
)


(def input-address-bar_ {:names ["input"] :target-type :web_address-bar})
(defn ^{:axon input-address-bar_} input-address-bar[target-eval param-evals 
														ctx globals & more]
	(let [target (deref-eval target-eval)
		  browser (@globals (:value target))
		  driver (when browser (:driver (:value @browser)))
		  url    (:value (deref-eval (:with param-evals)))
		 ]
		 (if driver
			(.get driver url)
            (throw (ex-info "Browser not open."))						
		 )
	)
)


(def read-address-bar_ {:names ["read"] :target-type :web_address-bar})
(defn ^{:axon read-address-bar_} read-address-bar[target-eval param-evals 
														ctx globals & more]
	(let [target (deref-eval target-eval)
		  browser (@globals (:value target))
		  driver (when browser (:driver (:value @browser)))
		 ]
		 (if driver
		 	{:value
				(.executeScript driver "return location.href;" (into-array []))
			 :type :string 
			}
            (throw (ex-info "Browser not open."))			
		 )
	)
)


(def show-html_ {:names ["show", "is-shown?"] :target-type :web_html})
(defn ^{:axon show-html_} show-html[target-eval param-evals 
														ctx globals & more]
	(let [target (deref-eval target-eval)
		  browser (@globals (:value target))
		  gs-name (str "gs-" ((:objref target-eval) 0))
		  x       (println "gs-name: " gs-name)
		  driver (when browser (:driver (:value @browser)))
		 ]
		 (if driver
		 	(let [  xpath-expr (str "//*[@" gs-name "]")
		 			elem (try (.findElement driver (By/xpath xpath-expr) )
		 					  (catch NoSuchElementException e 
		 					  		nil
		 					  )
		 				 )
		 		 ]
		 		 (if elem
		 		 	{:type :boolean :pass true :value true}
		 		 	{:type :boolean :pass false :value false}
		 		 )
		 	)
			(throw (ex-info "Browser not open."))					 	
		 )
	)
)
