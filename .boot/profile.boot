(deftask cider "CIDER profile"
   []
   (require 'boot.repl)
   (swap! @(resolve 'boot.repl/*default-dependencies*)
          concat '[[cider/cider-nrepl "0.18.0"]])
   (swap! @(resolve 'boot.repl/*default-middleware*)
          concat '[cider.nrepl/cider-middleware])
   identity)
