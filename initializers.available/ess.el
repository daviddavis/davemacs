(add-to-list 'load-path "~/.emacs.d/vendor/ess/")
(add-to-list 'load-path "~/.emacs.d/vendor/ess/lisp")

(autoload 'R-mode "ess-site" "" t nil)
(autoload 'R "ess-site" "" t nil)
(load-file "~/.emacs.d/vendor/ess/lisp/ess-site.el")

;; https://stat.ethz.ch/pipermail/ess-help/2009-February/005158.html
(setq ess-eval-visibly-p nil)

(define-key ess-mode-map "_" nil)
