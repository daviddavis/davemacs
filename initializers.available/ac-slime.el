(add-to-list 'load-path "~/.emacs.d/vendor/ac-slime")
(require 'ac-slime)
(add-hook 'slime-mode-hook 'set-up-slime-ac)
