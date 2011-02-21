;; ********************************************************************************
;; vines
;; ********************************************************************************

(defun find-non-escaped-paren (kind direction)
  (cond ((= direction -1) (re-search-backward kind))
        ((= direction 1) (search-forward kind) (backward-char))
        (else (error "direction must be -1 or 1")))
  (cond ((looking-back "\\\\" 1) (find-non-escaped-paren kind direction))))

(defun select-previous-paren ()
  (interactive)
  (find-non-escaped-paren "(" -1)
  (setq p1 (point))
  (forward-list)
  (select-points p1 (point)))

(defun select-next-paren ()
  (interactive)
  (cond ((looking-at "(") (forward-char)))
  (find-non-escaped-paren "(" 1)
  (setq p1 (point))
  (forward-list)
  (select-points p1 (point))) 

(defun shrink-selection (p1 p2)
  (interactive "*r")
  (and mark-active (select-points (+ p1 1) (- p2 1))))

(defun trim-selection (p1 p2)
  (interactive "*r")
  (undo-boundary)
  (let (deactivate-mark)
    (filter-buffer-substring (- p2 1) p2 t)
    (filter-buffer-substring p1 (+ p1 1) t)))

(global-set-key (kbd "<C-s-backspace>") 'trim-selection)
(global-set-key (kbd "s-(") 'select-previous-paren)
(global-set-key (kbd "s-)") 'select-next-paren)
(global-set-key (kbd "C-s-0") 'select-outer-paren)
(global-set-key (kbd "C-s-9") 'shrink-selection)



(defvar ido-make-buffer-list-hook nil)

(defun textmate-plus/get-ido-buffer-list ()
  (let* ((ido-process-ignore-lists t)
         ido-ignored-list
         (ido-current-buffers (ido-get-buffers-in-frames 'current))
         (not-visible-buffers (ido-make-buffer-list-1 (selected-frame) ido-current-buffers)))
    (append not-visible-buffers ido-current-buffers)))

(defun textmate-plus/get-project-buffer-alist (project-root)
  (let ((all-buffers (textmate-plus/get-ido-buffer-list))
        (project-root-regex (regexp-quote project-root))
        project-buffers-alist)

    (dolist (buffer (reverse all-buffers))
      (with-current-buffer buffer
        (when (and buffer-file-name
                   (equal project-root (textmate-project-root)))
          (aput 'project-buffers-alist (replace-regexp-in-string project-root-regex "" buffer-file-name) buffer))))
    project-buffers-alist))

(defun textmate-plus-list-project-buffers (&optional project-root)
  "Project buffers"
  (interactive)
  (let* ((project-root (or project-root (textmate-project-root)))
         (project-buffers (textmate-plus/get-project-buffer-alist project-root))
         (choices (mapcar 'car project-buffers))
         (choice (ido-completing-read (format "Buffer in project `%s': " project-root)
                                      choices))
         (buffer (aget project-buffers choice nil)))
    (when buffer
      (switch-to-buffer buffer))))

; (mapcar project-buffers-alist)
(defun textmate-plus-switch-to-project ()
  "Switch to project"
  (interactive)
  (let* ((projects-alist)
         (this-buffer (current-buffer))
         (this-project (textmate-project-root))
         (buffers (delete-if-not (lambda (buffer) (with-current-buffer buffer
                                               (and buffer-file-name
                                                    (textmate-project-root)
                                                    (not (equal this-buffer buffer))
                                                    (not (equal this-project (textmate-project-root))))))
                                 (buffer-list))))

    (dolist (buffer buffers)
      (let ((project-root (with-current-buffer buffer
                            (textmate-project-root))))
        (unless (assoc-default project-root projects-alist)
          (aput 'projects-alist project-root buffer))))

    (let* ((projects (append (mapcar (lambda (pair) (car pair)) projects-alist) (list this-project)))
           (project (ido-completing-read "Project: " projects)))
      (textmate-plus-list-project-buffers project))))

(defun textmate-ibuffer-current-project (&optional project-root)
  "Shows ibuffer filtered to current project"
  (interactive)

  (let ((project-root (or project-root (textmate-project-root))))
    (ibuffer nil (format "ibuffer listing for %S" project-root))
    (ibuffer-filter-by-filename project-root)
    (setq *textmate-project-root* project-root)))

(global-set-key (kbd "<C-s-268632066>") 'textmate-plus-list-project-buffers)
(global-set-key (kbd "<C-s-268632080>") 'textmate-plus-switch-to-project)
(global-set-key (kbd "C-x <C-s-268632066>") 'textmate-ibuffer-current-project)

(defun textmate-plus-quick-find-file ()
  (interactive)
  (save-excursion
    (let* ((root (textmate-project-root))
           (selection (if (region-active-p)
                          (buffer-substring (region-beginning) (region-end))
                        (thing-at-point 'filename)))
           (filename (replace-regexp-in-string "^/*\\.*[/:]" "" selection))
           (regex (textmate-plus-partial-matching-regex filename))
           (choices (remove-if-not (lambda (item) (string-match regex item)) (textmate-cached-project-files root)))
           (selected-file (cond ((= 1 (length choices)) (car choices))
                                ((= 0 (length choices)) (message (format "no results for '%s'" filename)) nil)
                                (t (textmate-completing-read "Find file: " choices)))))
      (and selected-file (find-file
                          (concat
                           (expand-file-name root) "/"
                           selected-file))))))

(defun swap-around-regex (regex)
  (interactive (list (read-string "Swap around text: ")))
  (when (region-active-p)
    (let* ((selection (filter-buffer-substring (region-beginning) (region-end) t))
          p1
          p2
          (new-string
           (with-temp-buffer
             (insert selection)
             (goto-char (point-min))
             (search-forward-regexp regex)
             (setq p2 (point))
             (search-backward-regexp regex)
             (setq p1 (point))
             (format "%s%s%s"
                     (filter-buffer-substring p2 (point-max))
                     (filter-buffer-substring p1 p2)
                     (filter-buffer-substring (point-min) p1)))))
      (insert new-string))))
(global-set-key (kbd "C-s-t") 'textmate-plus-quick-find-file)
(global-set-key (kbd "<C-s-268632084>") 'textmate-plus-quick-find-file)
