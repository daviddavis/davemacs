;;; Loading Function =================================================
(defvar ini-loaded ()
  "List of files loaded during initialization.")

(defvar ini-not-loaded ()
  "List of files that failed to load during initialization.")

(defun ini-try-load (inifn ext)
  "Attempt to load an ini-type elisp file."
  (let ((fn (concat ini-directory "/" inifn ext)))
    (if (file-readable-p fn)
        (progn
          (message (concat "Loading " inifn))
          (load-file fn)
          (setq ini-loaded (cons inifn ini-loaded)) ))))

(defun ini-load (inifn)
  "Load a ini-type elisp file"
  (cond ((ini-try-load inifn ".elc"))
        ((ini-try-load inifn ".el"))
        (t (setq ini-not-loaded (cons inifn ini-not-loaded))
           (message (concat inifn " not found")))))

;;; Now load all the ini-xxx files in the initialization directory
(let* ((ini-directory (concat dotfiles-dir "initializers.enabled"))
       (files (sort (directory-files ini-directory nil "^.*\\.el$") 'string<)))
  (while (not (null files))
    (ini-load (substring (car files) 0 -3))
    (setq files (cdr files)) ))

