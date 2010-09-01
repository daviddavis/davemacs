(require 'color-theme)

(let* ((theme-directory (concat dotfiles-dir "color-themes/"))
       (files (sort (directory-files theme-directory nil "^.*\\.el$") 'string<)))
  (dolist (file files)
    (let ((file (concat theme-directory file)))
      (cond ((file-readable-p (concat file "c"))
             (load-file (concat file "c")))
            (t (load-file file))))))
  
