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
   	(:use [generuse.lib.exec :only (deref-eval defaxon to-eval)])	
    (:import (org.openqa.selenium.firefox FirefoxDriver)
			 (org.openqa.selenium.chrome ChromeDriver)
			 (org.openqa.selenium.ie InternetExplorerDriver)
			 (org.openqa.selenium.safari SafariDriver)
			 (org.openqa.selenium WebElement)
   			 (org.openqa.selenium By NoSuchElementException Keys
   			 					  StaleElementReferenceException
   			 )
   			 (org.openqa.selenium.interactions Actions)
   			 (java.util.concurrent TimeUnit)
   			 (java.util Random)
   	)
   	(:require [clojure.string :as str]
			  [clojure.data.json :as json]		   		
   	)   	
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

(def short-wait-secs 2)

(defn set-driver-timeout [driver timeout]
	(-> driver .manage .timeouts (.implicitlyWait timeout TimeUnit/SECONDS))
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

	(let [default-timeout 10
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
		  timeout 		(try (Long/parseLong (:timeout obj)) 
		  					 (catch NumberFormatException e 
		  						default-timeout
		  					 )
		  	            )
		 ]
		(dosync (alter (:value browser-eval) assoc :timeout timeout))
		(set-driver-timeout driver timeout)
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

(defn is-web-element?[e]
	(instance? WebElement e)
)

(defn is-table?[e]
	(when (is-web-element? e) 
	 	  (.getAttribute e "gs-table")
	) 	
)

(defn small-delay[obj-eval globals]
	(try (Thread/sleep (* short-wait-secs 1000))
		(catch InterruptedException e nil)
	)
)

(defaxon :web_html ["show" "is-shown?"]
	(let [elem 	(locate-elem target-eval globals (= (:actor ctx) "_pre"))
		  value (:value (deref-eval (:with param-evals)))
		  check-value (fn []
						(if value
							(if (= (.getText elem) value)
					 			{:type Boolean :pass true :value true}
					 			{:type Boolean :pass false :value false}
					 		)
					 		{:type Boolean :pass true :value true}
					 	)		  	
		  			  )
		 ]
		(if (is-web-element? elem)
			(if (.isDisplayed elem)
				(check-value)
			 	(do
			 		(small-delay target-eval globals)
			 		(if (.isDisplayed elem)
			 			(check-value)
				 		{:type Boolean :pass false :value false 
				 	 	:reason "Element exists but not visible"}
			 	 	)
			 	)
		 	)
		 	{:type Boolean :pass false :value false :reason elem}
		)
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

(defn to-index[sel-spec]
	(condp = sel-spec
		"first" 	0
		"second" 	1
		(let [n (re-find #"\d+" sel-spec)
			  n (try (Long/parseLong n) (catch NumberFormatException e 0))
			 ]
			 n
		)
	)
)

(defn get-browser[obj-eval globals]
	(let[ obj 			(deref-eval obj-eval)
		  obj-val 		(:value obj)	
		  browser-name 	(if (string? obj-val) obj-val 
		  	            	(:browser obj-val)
		  	           	)
		  browser 		(@globals browser-name)
		]
		browser
	)
)

(defn get-web-driver [browser]
	(when browser (:driver (:value @browser)))
)

(defn locate-elem[obj-eval, globals, short-wait?]
	(let [obj 			(deref-eval obj-eval)
		  obj-val 		(:value obj)	
		  browser 		(get-browser obj-eval globals)		  
		  driver 		(get-web-driver browser)
		  refs          (if (:elem obj-val) (:subref obj-eval)
		  	                                (:objref obj-eval)
		  	            )
		  start-elem    (if (:elem obj-val) (:elem obj-val) driver)
		  n-refs 		(count refs)
		 ]
		 (when (not driver) (throw (ex-info "Browser not open" {})))
		 (when short-wait?
		 	(set-driver-timeout driver short-wait-secs)
		 )		 		 
	 	 (loop  [idx  0 elem start-elem]
	 		    (if (and (< idx n-refs) elem)
	 		    	(let [row-elems 
		 		    		(when (is-table? elem)
		 		    			(try (.findElements elem 
		 		    								(By/cssSelector "[gs-row]")
		 		    				 )
		  	 		  		   		 (catch NoSuchElementException e nil )
		  	 		  		   		 (catch StaleElementReferenceException e nil)
		  	 		  		   	)
		  	 		  		)
	 		    		]
		 		    	(if (is-table? elem)
		 		    		(if row-elems
	 		    				(let [row-idx (to-index (refs idx))
		 		    				  elem 
			 		    				  (try
			 		    					(.get row-elems row-idx)
			 		    					(catch IndexOutOfBoundsException e
			 		    							nil
			 		    					)
		 		    					  )
		 		    				]
	 		    					(recur (+ idx 1) elem)			 		    					
	 		    				)		 		    				
		 		    			(do (assert (> idx 0))
				 		    		(str "Expecting rows in " (refs (dec idx)) 
				 		    			 " but not found."
				 		    		)
			 		    		)	 		    			
		 		    		)
				 		  	(let [ sel (str "[gs=" (get-gs-name refs idx) "]")
			  	 		  			elem (try (.findElement elem 
			  	 		  									(By/cssSelector sel)
			  	 		  					   )
			  	 		  		   			 (catch NoSuchElementException e 
			  	 		  		   			 	nil
			  	 		  		   			 )
			  	 		  		   			 (catch StaleElementReferenceException e
			  	 		  		   			 	nil
			  	 		  		   			 )
			  	 		  		   	    )
			  	 		  		 ]
			 		  	  		(recur  (+ idx 1) elem)
			 		  		)
		 		    	)
	 		    	)
	  	
	 		  		(do
						(when short-wait?
		 					(set-driver-timeout driver (:timeout @browser))
		 				)		 		 
	 		  			elem
	 		  		)
 		  	  	)
	 	 ) 
	)	
)

(defaxon :web_html ["input"]														
	(let [input-vals    (:value (deref-eval (:with param-evals)))
		  input-vals 	(if input-vals input-vals param-evals)
		  elem 			(locate-elem target-eval globals false)
		 ]
		(if (is-web-element? elem)
			(do
				(cond 
		  	  		(string? input-vals)
		  	  		(do
		  	  			(try
							(.clear elem)
	 			  	  		(.sendKeys elem (into-array [input-vals]))
							(catch Exception e 
								{:type Boolean :value false :pass false
								 :reason (.getMessage e) :exception e
								}
							)	 			  	  		
 			  	  		)
 			  	  	)
		  	  		(map? input-vals)
		  	  		(input-batch elem input-vals)
			  	)
				{:type Boolean :value true :pass true}
		  	)
  	  		{:type Boolean :value false :pass false :reason elem}
  	  	)
	)
)


(defaxon :web_html ["enter"]
	(let [elem  (locate-elem target-eval globals false)]
		(if (is-web-element? elem)
			(do
				(try
					(.sendKeys elem (into-array [Keys/RETURN]))
					(catch Exception e 
						{:type Boolean :value false :pass false
						 :reason (.getMessage e) :exception e
						}
					)					
				)
				{:type Boolean :value true :pass true}				
			)
  	  		{:type Boolean :value false :pass false :reason elem}
  	  	)
	)
)

(defaxon :web_html ["click"]
	(let [input-val     (:value (deref-eval (:with param-evals)))	
		  elem 			(locate-elem target-eval globals false)
		  browser 		(get-browser target-eval globals)		  
		  driver 		(get-web-driver browser)
		  actions 		(when driver (Actions. driver))
		 ]
		 (when (not driver)
		 	(throw (ex-info "Browser not open" {}))
		 )
		 (if (is-web-element? elem)
			 (do
			 	(try
					(-> actions (.moveToElement elem) .click .build .perform)
					(catch Exception e 
						{:type Boolean :value false :pass false
						 :reason (.getMessage e) :exception e
						}
					)
				)
				{:type Boolean :value true :pass true}
		  	 )
  	  		 {:type Boolean :value false :pass false :reason elem}
  	  	 )
	)
)

(defaxon :web_html ["mouse-over"]
	(let [input-val     (:value (deref-eval (:with param-evals)))	
		  elem 			(locate-elem target-eval globals false)
		  browser 		(get-browser target-eval globals)		  
		  driver 		(get-web-driver browser)
		  actions 		(when driver (Actions. driver))
		 ]
		 (when (not driver)
		 	(throw (ex-info "Browser not open" {}))
		 )
		 (if (is-web-element? elem)
			 (do
			 	(try
					(-> actions (.moveToElement elem) .build .perform)
					(catch Exception e
						{:type Boolean :value true :pass false 
						 :reason (.getMessage e) :exception e}
					)
				)
				{:type Boolean :value true :pass true}
		  	 )
  	  		 {:type Boolean :value false :pass false :reason elem}
  	  	 )
	)
)

(defaxon :web_local-storage ["pick"]
	(let [target 		(deref-eval target-eval)
		  target-val 	(:value target)	
		  browser-name 	(if (string? target-val) target-val 
		  	            	(:browser target-val)
		  	           	)
		  browser 		(@globals browser-name)
		  driver 		(when browser (:driver (:value @browser)))
		  store-key     (:value (deref-eval (param-evals :with)))
		  script        (str "return window.localStorage.getItem('" store-key "');")
		  res 			(.executeScript driver  script (into-array []))
		  res           (if res 
		  					res 
		  					(do
		  						(small-delay)
								(.executeScript driver  script (into-array []))		  						
		  					)
		  				)
		]
		(if res
			(to-eval (try (json/read-str res)
						  (catch Exception e
						  	 res
						  )
					 )
			)
			{:type Boolean :value false :pass false 
			 :reason (str "Local storage lookup failed for key: " store-key)}
		)
	)
)

(defaxon :web_html ["pick-any"]
	(let [elem 			 (locate-elem target-eval globals false)
		  target-value   (:value (deref-eval target-eval))
		  browser-name   (if (string? target-value) target-value 
		 					(:browser target-value)
		 				 )
		 ]
		(if (is-table? elem)
			(do
				(let [rows
						(try (.findElements elem 
											(By/cssSelector "[gs-row]")
							 )
			  		   		 (catch NoSuchElementException e nil)
			  		   		 (catch StaleElementReferenceException e nil)
			  		   	)
		  		     rnd (Random. (System/currentTimeMillis))
			  		]
			  		(if rows
			  			(do
		  				    (def idx (.nextInt rnd (count rows)))
		  				    {
		  				    	:value {:elem (.get rows idx)
		  				    			:browser browser-name
		  				    		   }
		  				    	:type :web_html
		  				    }
				  		)
					 	{:type Boolean :pass false :value false 
					 	 :reason (str "Web table has no rows: " 
					 	 	          (str/join "'s" (:objref target-eval))
					 	 	     )
					 	}		  			
			  		)			  		
		  		)
			)
		 	{:type Boolean :pass false :value false 
		 	 :reason (str "Web element is not table: " 
		 	 	          (str/join "'s" (:objref target-eval))
		 	 	     )
		 	}
		)
	)
)

(defaxon :web_html ["remove"]
	(let [elem 			 (locate-elem target-eval globals false)
		  browser 		 (get-browser target-eval globals)		  
		  driver 		 (get-web-driver browser)
		  elem 			 (when elem
 								(set-driver-timeout driver short-wait-secs)
		  						(try (.getLocation elem)
		  							 (catch StaleElementReferenceException e
		  								nil
		  							 )
		  							 (finally
		 								(set-driver-timeout driver 
		 													(:timeout @browser)
		 								)
		  							 )
		  						)
		  				 )
		 ]
		 (if elem
		 	{:type Boolean :value false :pass false}
		 	{:type Boolean :value true :pass true}
		 )
    )
)