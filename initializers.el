(defun ini-mtime (filename)
  "Return the mtime of the specified filename, following symlinks as appropriate"
  (let* ((attributes (file-attributes filename))
         (mtime (nth 5 attributes))
         (symlink-dest (nth 0 attributes)))
    (if symlink-dest
        (ini-mtime (concat (file-name-directory filename) symlink-dest))
      (+ (* (car mtime) 65536) (cadr mtime)))))

(defun ini-pick-latest-version (filename)
  "Given a .el file, compare the mtime of the corresponding .elc file, following symlinks to get the mtime

   If the .elc file is out-dated, delete it"
  (let ((elc-filename (concat filename "c")))
    (if (file-readable-p elc-filename)
        (if (> (ini-mtime filename) (ini-mtime elc-filename))
            (progn (message "Detected that %S is newer than %S. Deleting %S" filename elc-filename elc-filename)
                   (delete-file elc-filename)
                   filename)
          elc-filename)
      filename)))

(defun ini-load (filename)
  (load-file (ini-pick-latest-version filename)))

;;; load all the files in the initializers.enabled/ directory
(let* ((ini-directory (concat dotfiles-dir "initializers.enabled/"))
       (files (sort (directory-files ini-directory nil "^.*\\.el$") 'string<)))
  (dolist (file files)
    (ini-load (concat ini-directory file))))

