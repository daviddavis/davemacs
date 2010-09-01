(eval-after-load 'ruby-mode
  '(progn
     (require 'rspec-mode)
     (require 'ruby-electric)

     (add-hook 'ruby-mode-hook (lambda () (ruby-electric-mode t)))

     (defun ruby-eval-region-or-last-sexp ()
       "if region active, evaluate it. otherwise last sexp"
       (interactive)
       (if mark-active
           (let (deactivate-mark)
             (ruby-send-region (region-beginning) (region-end)))
         (ruby-send-last-sexp)))

     (define-key ruby-mode-map (kbd "C-i") 'ruby-eval-region-or-last-sexp)
     (define-key ruby-mode-map (kbd "C-I") 'ruby-send-definition)
     (define-key ruby-mode-map (kbd "C-c l") 'ruby-send-line)
     (define-key ruby-mode-map (kbd "C-c C-e") 'ruby-send-last-sexp)
     (define-key ruby-mode-map (kbd "C-c C-d") 'ruby-send-definition)
     (define-key ruby-mode-map (kbd "TAB") nil)
     (define-key ruby-mode-map (kbd "M-TAB") 'inf-ruby-bond-complete-or-tab)

     (defun inf-ruby187 ()
       (interactive)
       "Runs rvm-use, then inf-ruby"
       (rvm-use "ruby-1.8.7-p249" "*default*")
       (inf-ruby))

     (defun inf-ruby192 ()
       (interactive)
       "Runs rvm-use, then inf-ruby"
       (rvm-use "ruby-1.9.2-preview3" "*default*")
       (inf-ruby))

     (defun bundler-find-Gemfile-root (&optional directory)
       "Finds the Gemfile of the project by walking the directory tree until it finds a Gemfile."
       (let ((directory (file-name-as-directory (or directory default-directory))))
         (cond ((rspec-root-directory-p directory) nil)
               ((file-exists-p (concat directory "Gemfile")) directory)
               (t (bundler-find-Gemfile-root (file-name-directory (directory-file-name directory)))))))

     (defun bundler-gems-list ()
       "Lists gems used by bundler"
       (let ((bundler-root (bundler-find-Gemfile-root)))
         (when (not bundler-root)
           (message  "Can't find bundler Gemfile")
           (signal 'quit nil ))
         (let* ((lines (split-string (shell-command-to-string (format "cd %S && bundle list" bundler-root)) "\n"))
                (gem-lines (rest lines))
                (gems (mapcar (lambda (line)
                                (replace-regexp-in-string "^ *\\* *\\(.*?\\) (.+) *$" "\\1" line))
                              gem-lines)))
           gems)))

     (defun bundler-open-gem (gem-name)
       (interactive (bundler-prompt-for-gem))
       (let ((gem-path (shell-command-to-string
                        (format "cd %S && bundle show %S"
                                (bundler-find-Gemfile-root)
                                gem-name))))
         (dired (format "%s/" (replace-regexp-in-string "\n*$" "" gem-path)))))

     (defun bundler-prompt-for-gem ()
       (let ((response (ido-completing-read "Gem: "
                                            (bundler-gems-list))))
         (when response (list response))))



     ))
