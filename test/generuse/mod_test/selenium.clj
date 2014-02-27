(ns generuse.mod-test.selenium
  (:require [clojure.test :refer :all]
            [generuse.mod.selenium :refer :all]))

(deftest basic-test
  (testing "basic test"
  	(let [ site-url "http://cumulus.generuse.com"
           globals (ref {
  							"browser" (ref {:type :web_browser} )
  							"address-bar" (ref {:type :web_address-bar 
  								           :value "browser"})
  						    "signin-page" (ref {:type :web_html :value "browser"}
  						    	          )  						    
  						}
  				   )
  	       ctx    {}
  	      ]

  	      (open-browser {:type :heap-obj 
  	      	             :objref ["browser"] 
  	      	             :value (@globals "browser") }
  	      	             {}
  	      	             ctx
  	      	             globals 
  	      )

          (let [reval (show-browser {:type :heap-obj
                         :objref ["browser"] 
                         :value (@globals "browser") }
                         {}
                         ctx
                         globals 
                    ) ]
                 (assert (:value reval))
          )

          (input-address-bar {:type :heap-obj 
                         :objref ["address-bar"] 
                         :value (@globals "address-bar") }
                         {:with {:value site-url :type :string}}
                         ctx
                         globals 
          )

          (let [reval (read-address-bar {:type :heap-obj
                         :objref ["address-bar"] 
                         :value (@globals "address-bar") }
                         {}
                         ctx
                         globals 
                    ) ]
                 (assert (.startsWith (:value reval) site-url))
          )

          (let [reval (show-html {:type :heap-obj
                         :objref ["signin-page"] 
                         :value (@globals "signin-page") }
                         {}
                         ctx
                         globals 
                    ) ]
                 (assert (:value reval))
          )

          (input-html {:type :heap-obj 
                         :objref ["signin-page" "new-user"] 
                         :value (@globals "signin-page") }
                         {:with {:value {"email" "x@y.com" 
                                         "password" "test"
                                         "password-confirm" "test"} 
                                :type :map}
                          }
                         ctx
                         globals 
          )

          true
  	)
  )
 )
