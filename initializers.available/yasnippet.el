(require 'yasnippet)

(setq yas/root-directory (concat dotfiles-dir "snippets"))
(yas/initialize)
(yas/load-directory (concat dotfiles-dir "snippets"))
