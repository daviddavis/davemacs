(require 'linum)
(setq linum-format " %3d")
(global-set-key (kbd "M-n") 'linum-mode)

;; install hooks for various langauges to automatically turn on line numbers
(defun turn-on-linum-mode () (linum-mode 1))
(setq linum-modes
      '(emacs-lisp-mode
        clojure-mode
        scheme-mode
        R-mode
        ruby-mode
        html-mode
        yaml-mode
        espresso-mode
        feature-mode))
(dolist (mode linum-modes)
  (add-hook (intern (concat (symbol-name mode) "-hook")) 'turn-on-linum-mode))

