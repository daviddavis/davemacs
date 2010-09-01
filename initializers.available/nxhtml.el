;;; nxml (HTML ERB template support)
(defvar nxhtml-autostart-file "~/.emacs.d/vendor/nxhtml/autostart.el")
(autoload 'eruby-nxhtml-mumamo-mode nxhtml-autostart-file
  "Turn on multiple major modes for eRuby with main mode `nxhtml-mode'. This also covers inlined style and javascript." t)

(eval-after-load nxhtml-autostart-file
  '(setq
    nxhtml-global-minor-mode t
    mumamo-chunk-coloring 'submode-colored
    nxhtml-skip-welcome t
    indent-region-mode t
    rng-nxml-auto-validate-flag nil
    nxml-degraded t))

;; (add-to-list 'auto-mode-alist '("\\.html\\.erb\\'" . 'eruby-nxhtml-mumamo-mode))
