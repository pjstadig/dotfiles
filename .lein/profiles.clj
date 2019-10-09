{:user {:dependencies [[pjstadig/humane-test-output "0.8.3"]]
        :injections [(require 'pjstadig.humane-test-output)
                     (pjstadig.humane-test-output/activate!)]
        :plugins [[lein-ancient "0.6.12"]
                  [lein-pprint "1.2.0"]]}}
