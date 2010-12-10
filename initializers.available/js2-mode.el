;; If you prefer js2-mode, use this instead:
(add-to-list 'load-path "~/.emacs.d/vendor/js2-mode/")
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(eval-after-load 'js2-mode
  '(progn
     (define-key js2-mode-map (kbd "C-M-h") nil)))

