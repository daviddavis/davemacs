; In order to use this you must first 
; 1. run "ant" in the vendor/rsense.  (This assumes you have cloned the submodules.)
; 2. Set RSENSE_HOME to ~/.emacs.d/vendor/rsense in your shell file.
; 3. After 2, run "ruby ~/.emacs.d/vendor/rsense/etc/config.rb > ~/.rsense"
; More info on RSense: http://cx4a.org/software/rsense/manual.html
(setq rsense-home (expand-file-name "~/.emacs.d/vendor/rsense"))
;(setq rsense-home "$RSENSE_HOME")
(add-to-list 'load-path (concat rsense-home "/etc"))
(require 'rsense)

(add-hook 'ruby-mode-hook
          (lambda ()
            (add-to-list 'ac-sources 'ac-source-rsense-method)
            (add-to-list 'ac-sources 'ac-source-rsense-constant)))
