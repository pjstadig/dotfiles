;; OSX finder launched applications...environment...grumble.  If I
;; want to be able to jack in with cider, it needs to be able to find
;; lein on the path.  This is a no-op on nixos.
(add-to-list 'exec-path (expand-file-name "~/bin"))
