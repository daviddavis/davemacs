(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(LaTeX-command "latex")
 '(ack-project-root-file-patterns (quote (".project\\'" ".xcodeproj\\'" ".sln\\'" "\\`Project.ede\\'" "\\`.git\\'" "\\`.bzr\\'" "\\`_darcs\\'" "\\`.hg\\'" ".dir-locals" ".emacs-project")))
 '(blink-cursor-mode t)
 '(css-indent-offset 2)
 '(cua-enable-cua-keys nil)
 '(cua-mode t nil (cua-base))
 '(cursor-type 'bar)
 '(erc-autojoin-channels-alist (quote (("freenode.net"))))
 '(erc-autojoin-mode t)
 '(erc-modules (quote (autojoin button completion fill irccontrols list log match menu move-to-prompt netsplit networks noncommands readonly ring stamp track)))
 '(exec-path (quote ("/Users/timcharper/bin" "/usr/bin" "/bin" "/usr/sbin" "/sbin" "/usr/local/bin" "/usr/X11/bin" "/usr/local/git/bin" "/Applications/Emacs.app/Contents/MacOS/bin" "/usr/local/texlive/2009/bin/universal-darwin/" "/opt/local/bin")))
 '(ffap-machine-p-known (quote reject)) ; so ido-use-filename-at-point doesn't try to ping strange domains
 '(global-auto-revert-mode t)
 '(gnuserv-program (concat exec-directory "/gnuserv"))
 '(hl-paren-background-colors nil)
 '(hl-paren-colors (quote ("#ff2244" "#ff7700" "#ffff00" "#00ff00" "#2266ff" "#00aaff" "#ff00ff" "black")))
 '(ido-auto-merge-delay-time 60)
 '(ido-enable-flex-matching t)
 '(ido-mode (quote both) nil (ido))
 '(imenu-max-item-length 120)
 '(init-face-from-resources nil)
 '(load-home-init-file t t)
 '(make-backup-files nil)
 '(mouse-wheel-progressive-speed t)
 '(mouse-wheel-scroll-amount (quote (1 ((shift) . 1) ((control)))))
 '(preview-transparent-color "black")
 '(quack-remap-find-file-bindings-p nil)
 '(rails-indent-and-complete nil)
 '(ruby-electric-expand-delimiters-list (quote (123 91 40 39 34 96)))
 '(safe-local-variable-values (quote ((ack-arguments "--ignore-dir=ruby") (*textmate-gf-exclude* . "/\\.|vendor|ruby/gems|classes|fixtures|tmp|log|build|\\.xcodeproj|\\.nib|\\.framework|\\.app|\\.pbproj|\\.pbxproj|\\.xcode|\\.xcodeproj|\\.bundle|\\.pyc") (ruby-compilation-executable . "ruby") (ruby-compilation-executable . "ruby1.8") (ruby-compilation-executable . "ruby1.9") (ruby-compilation-executable . "rbx") (ruby-compilation-executable . "jruby"))))
 '(sentence-end-double-space nil)
 '(sh-basic-offset 2)
 '(sh-indentation 2)
 '(sh-shell-file "/bin/bash")
 '(shell-file-name "/bin/bash")
 '(show-paren-mode t)
 '(tab-width 2)
 '(tex-dvi-view-command (quote open))
 '(textile-browser-command (quote ("open" "-a" "Safari")))
 '(tls-program (quote ("openssl s_client -connect %h:%p -no_ssl2 -ign_eof")))
 '(toolbar-mail-reader (quote gnus))
 '(toolbar-news-reader (quote gnus))
 '(truncate-lines t))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

(load (concat dotfiles-dir "initializers.el"))

(setenv "PATH" (format "%s/bin:%s/Developer/bin:%s"
                       (getenv "HOME")
                       (getenv "HOME")
                       (getenv "PATH")))

(server-start)
