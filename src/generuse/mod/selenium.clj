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
   	(:use [generuse.lib.exec :only (deref-eval defaxon)])	
    (:import (org.openqa.selenium.firefox FirefoxDriver)
			 (org.openqa.selenium.chrome ChromeDriver)
			 (org.openqa.selenium.ie InternetExplorerDriver)
			 (org.openqa.selenium.safari SafariDriver)
   			 (org.openqa.selenium By NoSuchElementException Keys)
   			 (java.util.concurrent TimeUnit)
   	)
   	(:require [clojure.string :as str])   	
)

(declare locate-elem)

(def web-drivers 
	{
		"firefox"	#(FirefoxDriver.)
		"chrome"  	#(ChromeDriver.)
		"safari"  	#(SafariDriver.)
		"explorer"	#(InternetExplorerDriver.)
	}
)

(defn create-driver [browser-eval init-value globals]
	(def obj (deref-eval browser-eval))
	(when (= init-value "chrome")
		  (if (:driver obj)
		  	  (System/setProperty "webdriver.chrome.driver" (:driver obj))
		  	  (throw (ex-info "The property 'driver' must point to chrome driver exe "
		  	  	               "in gso file"
		  	  	     )
		  	  )
		  )
	)

	(let [
		  driver 		(if (web-drivers init-value) 
		  					((web-drivers init-value))
				            (throw (ex-info (str "WebDriver for: " init-value 
				                                 " not found. Supported: " 
				                                 (str/join "," 
				                                 	       (keys web-drivers)
				                                  )
				                            )
				            				{}
				                    )
				            )		  					
		  				)		  
		  timeout-param (try (Long/parseLong (:timeout obj)) 
		  					 (catch NumberFormatException e 
		  						    nil
		  					 )
		  	            )
		  timeout       (if timeout-param timeout-param 10)
		 ]
		(-> driver .manage .timeouts (.implicitlyWait timeout TimeUnit/SECONDS))
		driver
	)
)

(defaxon :web_browser ["open"]
	(let [target-obj    (deref-eval target-eval)
		  init-value 	(if (string? (:value target-obj))
		  					(:value target-obj)
		  					(:init-value target-obj)
		  	            )
		  driver 		(create-driver target-eval init-value globals)		  
		 ]
		(dosync
		    (alter (:value target-eval) 
		    		assoc 
		    		:value {:driver driver}
 					:init-value init-value		    		
		    ) 
		)
	)
)

(defaxon :web_browser ["show" "is-shown?"]
	(let [driver (get-in (deref-eval target-eval) [:value :driver] )]
		(if (and driver (not (.contains (.toString driver) "null")))
			{:value true :type Boolean :pass true}
			{:value false :type Boolean :pass false}			
		)
	)
)


(defaxon :web_address-bar ["input"]
	(let [target (deref-eval target-eval)
		  target-val (:value target)		
		  browser-name (if (string? target-val) target-val 
		  	               (:browser target-val)
		  	           )
		  browser (@globals browser-name)
		  driver (when browser (:driver (:value @browser)))
		  url    (:value (deref-eval (:with param-evals)))
		 ]
		 (if driver
			(.get driver url)
            (throw (ex-info "Browser not open."))						
		 )
	)
)

(defaxon :web_address-bar ["read"]														
	(let [target (deref-eval target-eval)
		  target-val (:value target)
		  browser-name (if (string? target-val) target-val 
		  	               (:browser target-val)
		  	           )
		  browser (@globals browser-name)
		  driver (when browser (:driver (:value @browser)))
		 ]
		 (if driver
		 	{:value
				(.executeScript driver "return location.href;" (into-array []))
			 :type String
			}
            (throw (ex-info "Browser not open." {}))			
		 )
	)
)

(defn get-gs-name[refs idx]
	(refs idx)
)

(defaxon :web_html ["show" "is-shown?"]
	(if (locate-elem target-eval globals)
	 	{:type Boolean :pass true :value true}
	 	{:type Boolean :pass false :value false}
	)
)


(defn input-batch[elem vals]
	(let [sel 	"[gs]"
		  elems (.findElements elem (By/cssSelector sel))
		 ]
		 (doall 
		 	(map
		 		#(let [name 	 (.getAttribute % "gs")
		 			   input-val (vals name)
		 			   input-val (if (string? input-val) 
		 			   				 input-val 
		 			   				 (:value (deref-eval input-val))
		 			   			 )
		 			  ]
		 			  (when input-val
		 			  		(.clear %)
		 			  	  	(.sendKeys % (into-array [input-val]))
		 			  )
		 		 )
		 		elems
		 	)
		 )
	) 
)

(defn locate-elem[obj-eval, globals]
	(let [obj 			(deref-eval obj-eval)
		  obj-val 		(:value obj)	
		  browser-name 	(if (string? obj-val) obj-val 
		  	            	(:browser obj-val)
		  	           	)
		  browser 		(@globals browser-name)
		  driver 		(when browser (:driver (:value @browser)))
		  refs          (if (:elem obj-val) (:subref obj-eval)
		  	                                   (:objref obj-eval)
		  	            )
		  start-elem    (if (:elem obj-val) (:elem obj-val) driver)
		  n-refs 		(count refs)
		 ]		 
		 (when (not driver) (throw (ex-info "Browser not open")))
	 	 (loop  [idx  0 elem start-elem]
	 		    (if (and (< idx n-refs) elem)
	  	 		  	(let [ sel  (str "[gs=" (get-gs-name refs idx) "]")
	  	 		  		   elem (try (.findElement elem (By/cssSelector sel))
	  	 		  		   			 (catch NoSuchElementException e 
	  	 		  		   			 	nil
	  	 		  		   			 )
	  	 		  		   	    )
	  	 		  		 ]
	 		  	  		(recur  (+ idx 1) elem)
	 		  		)
	 		  		elem
 		  	  	)
	 	 ) 
	)	
)

(defaxon :web_html ["input"]														
	(let [input-vals    (:value (deref-eval (:with param-evals)))
		  input-vals 	(if input-vals input-vals param-evals)
		  elem 			(locate-elem target-eval globals)
		 ]
		(if elem
			(do
				(cond 
		  	  		(string? input-vals)
		  	  		(do
						(.clear elem)
 			  	  		(.sendKeys elem (into-array [input-vals]))
 			  	  	)
		  	  		(map? input-vals)
		  	  		(input-batch elem input-vals)
			  	)
				{:type Boolean :value true :pass true}
		  	)
  	  		{:type Boolean :value false :pass false}
  	  	)
	)
)

(defaxon :web_html ["click"]
	(let [input-val     (:value (deref-eval (:with param-evals)))	
		  elem 			(locate-elem target-eval globals)
		 ]
		(if elem
			(do
				(.click elem)
				{:type Boolean :value true :pass true}
		  	)
  	  		{:type Boolean :value false :pass false}
  	  	)
	)
)