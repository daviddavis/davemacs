;;; ==================================================================
;;; Author:  Jim Weirich
;;; File:    tags
;;; Purpose: Extended find-tag support.
;;; ==================================================================

(require 'etags)

(defun jw-ft-be-prefix-transform (tagname)
  "Transform the tagname if it matches the 'be_xxxx' pattern."
  (and tagname
       (> (length tagname) 3)
       (string= (substring tagname 0 3) "be_")
       (substring tagname 3) ))

(defun jw-ft-transform (tagname)
  "Transform the extended tagname to its actual name, or nil if there is no transform."
  (jw-ft-be-prefix-transform tagname))

(defun jw-ft-extended-find (tagname next-p regexp-p)
  "Call find-tag with a tranformed tagname."
  (let ((transformed-tagname (jw-ft-transform tagname)))
    (if transformed-tagname
        (find-tag transformed-tagname next-p regexp-p)
      (error (concat "No extended tags containing " tagname)) )))

;; (defun jw-extended-find-tag (tagname &optional next-p regexp-p)
;;   "Extended find-tag function to handle tags that don't literally match."
;;   (interactive (find-tag-interactive "Find extended tag: "))
;;   (condition-case nil
;;       (find-tag tagname next-p regexp-p)
;;     (error (jw-ft-extended-find tagname next-p regexp-p)) ))
;;; Remap the standard find-tag key to use the extended version.
;; (global-set-key "\M-." 'jw-extended-find-tag)

(global-set-key "\M-." 'find-tag)


;;; -- My commands

(defcustom path-to-ctags
  "/usr/local/bin/ctags"
  "The path to your ctags instance")

(setq path-to-rscript "/Library/Frameworks/R.framework/Resources/bin/Rscript")

(defun create-rtags (dir-name)
  "Create tags file for R."
  (interactive "DDirectory: ")
  (shell-command
   (format "%s -f %s/TAGS -e -R %S" path-to-ctags dir-name dir-name))
  (shell-command
   (format "%s -e \"rtags('%s', recursive=TRUE, append=TRUE, ofile='%s/TAGS')\"" path-to-rscript dir-name dir-name))
  )

(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (let ((old-dir default-directory))

    (cd dir-name)
    (shell-command
     (format "%s -f TAGS -e -R ." path-to-ctags))))

(defun find-tags-file ()
  "recursively searches each parent directory for a file named `tags' and returns the
path to that file or nil if a tags file is not found. Returns nil if the buffer is
not visiting a file"
  (labels
      ((find-tags-file-r (path)
         (let* ((parent (file-name-directory path))
                (possible-tags-file (concat parent "tags")))
           (cond
             ((file-exists-p possible-tags-file) (throw 'found-it possible-tags-file))
             ((string= "/tags" possible-tags-file) (error "no tags file found"))
             (t (find-tags-file-r (directory-file-name parent)))))))

    (if (buffer-file-name)
        (catch 'found-it 
          (find-tags-file-r (buffer-file-name)))
        (error "buffer is not visiting a file"))))

(defun dwim-visit-tags ()
  "calls `jds-find-tags-file' to recursively search up the directory tree to find
a file named `tags'. If found, calls `vtags-set-tags-file' with that path as an argument
otherwise raises an error."
  (interactive)
  (visit-tags-table (jds-find-tags-file)))
