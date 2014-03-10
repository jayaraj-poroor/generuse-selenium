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
	(let [ is-check?  (when (= (:action ctx) "show") true)
		   driver (get-in (deref-eval target-eval) [:value :driver] ) ]
		(if (and driver (not (.contains (.toString driver) "null")))
			{:value true :type Boolean :pass (when is-check? true)}
			{:value false :type Boolean :pass (when is-check? false)}
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

(defn is-web-element?[e]
	(instance? WebElement e)
)

(defn is-table?[e]
	(when (is-web-element? e) 
	 	  (.getAttribute e "gs-table")
	) 	
)

(defn is-row?[e]
	(when (is-web-element? e)
		(.getAttribute e "gs-row")
	)
)

(defn is-gs-elem?[e]
	(when (is-web-element? e)
		(.getAttribute e "gs")
	)
)

(defn small-delay[obj-eval globals]
	(try (Thread/sleep (* short-wait-secs 1000))
		(catch InterruptedException e nil)
	)
)

(defn style-matches? [elem param-evals]
	(every?
		#(let [param-name (first %) param-val (-> % second deref-eval :value)]
			(if (keyword? param-name)
				true
				(= (.getCssValue elem param-name) param-val)
			)
		)
		param-evals
	)
)

(defn find-children[elem]
	(try (.findElements elem 
						(By/xpath "*")
		 )
   		 (catch NoSuchElementException e nil )
   		 (catch StaleElementReferenceException e nil)
   	)
)

(defn bfs-elems [elem is-expected-elem?]
	(loop [ret [] queue (conj clojure.lang.PersistentQueue/EMPTY elem)]
    	(if (seq queue)
      		(let [elem-node (peek queue)
      			  child-nodes (when (not (is-expected-elem? elem-node)) 
      			  					(find-children elem-node)
      			  			  )
      			 ]
      			 (if child-nodes
        			(recur ret (into (pop queue) child-nodes))
        			(recur (conj ret elem-node) (pop queue))
        		 )
        	)
      		ret
      	)
    )
)

(defn find-rows[elem]
	(bfs-elems elem is-row?)
)
;	(try (.findElements elem 
;						(By/cssSelector "[gs-row]")
;		 )
;  		 (catch NoSuchElementException e nil )
;   		 (catch StaleElementReferenceException e nil)
;   	)

(defn find-gs-child-elems[elem]
	(bfs-elems elem is-gs-elem?)
)
;	(.findElements elem (By/cssSelector "[gs]"))

(defn find-displayed-rows[elem]
	(into []
		(filter
			#(.isDisplayed %)
			(find-rows elem)
		)
	)
)

(defn find-elem[elem child-name]
	(let [	sel  (str "[gs=" child-name "]")
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
		elem
	)
)

(defn find-displayed-elem[elem child-name]
	(let [child (find-elem elem child-name)]
		(if (and child (.isDisplayed child))
			child
			nil
		)
	)
)

(defmulti content-matches?
		  (fn[elem content-eval param-evals reason] 
		  	 (:type (deref-eval content-eval))
		  )
)

(defmethod content-matches? String [elem content-eval param-evals reason]
	(let [r (= (.getText elem) (:value (deref-eval content-eval)))]
		(and r (style-matches? elem param-evals))
	)
)

(defmethod content-matches? Boolean [elem content-eval param-evals reason]
	(let [r (= (.isSelected elem) (:value (deref-eval content-eval)))]
		r
	)
)

(defmethod content-matches? :map [elem content-eval param-evals reason]
	(every?
		#(let [ename (first %) evalue (second %)]
			(if (keyword? ename) ;ignore :type entry 
				true
				(let [child (find-elem elem ename)]
					(when child
						(content-matches? child evalue param-evals reason)
					)
				)
			)
		)
		content-eval		
	)
)

(defmethod content-matches? :seq [elem content-eval param-evals reason]
	(if (is-table? elem)
		(let [rows (find-displayed-rows elem) row-idx (atom 0) n-rows (count rows)]
			(every?
				#(let [r (when (< @row-idx n-rows)
					   		   (content-matches? (nth rows @row-idx) % param-evals reason)
					     )
					]
					(swap! row-idx inc)
					r
				)
				(:value (deref-eval content-eval))
			)
		)
		(do
			(reset! reason "Element not a table")
			false
		)
	)
)

(defmethod content-matches? :nil [elem content-eval param-evals reason]
	(if (is-table? elem)
		(= (count (find-displayed-rows elem)) 0)
		(do
			(reset! reason "Matching nil content to a non-table element")
			false
		)
	)
)

(defmethod content-matches? :default [elem content-eval param-evals reason]
	(reset! reason (str "Unsupported content type: " 
		                (:type (deref-eval content-eval)) 
		            )
	)
	false
)

(defaxon :web_html ["show" "is-shown?"]
	(let [is-check?  (when (= (:action ctx) "show") true)
		  elem 		 (locate-elem target-eval globals (= (:actor ctx) "_pre"))
		  with-param (deref-eval (:with param-evals))
		  check-value (fn []
						(if with-param
					 		(let [reason (atom nil)
					 			  res (content-matches? elem with-param param-evals reason)]
					 			{:type Boolean :pass (when is-check? res)
					 			 :value res :reason @reason}
					 		)							
					 		(let [res (style-matches? elem param-evals)]
					 			{:type Boolean :pass (when is-check? res)
					 			 :value res}
					 		)
					 	)		  	
		  			  )
		 ]
		(if (is-web-element? elem)
			(if (.isDisplayed elem)
				(do
					(let [r-eval (check-value)]
						(if (:value r-eval)
							r-eval
							(do
								(small-delay target-eval globals)
								(check-value)
							)
						)
					)
			 	)
			 	(do
			 		(small-delay target-eval globals)
			 		(if (.isDisplayed elem)
			 			(check-value)
				 		{:type Boolean :pass (when is-check? false)
				 		 :value false 
				 	 	 :reason "Element exists but not visible"}
			 	 	)
			 	)
		 	)
		 	{:type Boolean :pass (when is-check? false)
		 	 :value false :reason elem}
		)
	)
)


(defn input-batch[elem vals]
	(let [elems (find-gs-child-elems elem)]
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

(defn to-index[sel-spec row-elems]
	(condp = sel-spec
		"first" 	0
		"second" 	1
		"last"      (dec (count row-elems))
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
		  	                                (:supref obj-eval)
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
	 		    	(let [row-elems (when (is-table? elem) (find-displayed-rows elem)) ]
		 		    	(if (is-table? elem)
		 		    		(if row-elems
	 		    				(let [row-idx (to-index (refs idx) row-elems)
		 		    				  elem 
			 		    				  (try
			 		    					(nth row-elems row-idx)
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
				 		  	(let [ elem (find-displayed-elem elem (refs idx))]
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
		  						(small-delay target-eval globals)
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
			{:type :nil :value nil}
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
				(let [rows (find-displayed-rows elem)
		  		      rnd (Random. (System/currentTimeMillis))
			  		]
			  		(if (seq rows)
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
					 	 	          (str/join "'s" (:supref target-eval))
					 	 	     )
					 	}		  			
			  		)			  		
		  		)
			)
		 	{:type Boolean :pass false :value false 
		 	 :reason (str "Web element is not table: " 
		 	 	          (str/join "'s" (:supref target-eval))
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

(defaxon :web_html ["read"]
	(let [elem 	(locate-elem target-eval globals (= (:actor ctx) "_pre"))]
		(if (is-web-element? elem)
			{:value (.getText elem) :type String}
		 	{:type Boolean :pass false :value false :reason elem}
		)
	)
)

(defaxon :web_html ["select" "is-selected?"]
	(let [elem 	(locate-elem target-eval globals (= (:actor ctx) "_pre"))]
		(if (is-web-element? elem)
			(let [is-selected? (.isSelected elem)]
				{:type Boolean :value is-selected? :pass is-selected?}
			)
		 	{:type Boolean :pass false :value false :reason elem}
		)
	)
)

