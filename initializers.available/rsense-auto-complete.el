; In order to use this you must first run "ant" in the vendor/rsense.  (This assumes you have cloned the submodules.)
; More info on RSense: http://cx4a.org/software/rsense/manual.html
(setq rsense-home "~/.emacs.d/vendor/rsense")
;(setq rsense-home "$RSENSE_HOME")
(add-to-list 'load-path (concat rsense-home "/etc"))
(require 'rsense)

(add-hook 'ruby-mode-hook
          (lambda ()
            (add-to-list 'ac-sources 'ac-source-rsense-method)
            (add-to-list 'ac-sources 'ac-source-rsense-constant)))
