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

(global-set-key (kbd "C-S-d") 'trim-selection)
(global-set-key (kbd "s-(") 'select-previous-paren)
(global-set-key (kbd "s-)") 'select-next-paren)
(global-set-key (kbd "C-s-0") 'select-outer-paren)
(global-set-key (kbd "C-s-9") 'shrink-selection)



(defvar ido-make-buffer-list-hook nil)

(defun textmate-plus-list-project-buffers (&optional project-root)
  "Project buffers"
  (interactive)
  (let* ((project-root (or project-root (textmate-project-root)))
         (filter-fn (lambda ()
                      (setq prompt (format "Buffer in project `%s': " (textmate-project-root)))
                      (setq ido-temp-list (delete-if-not (lambda (buffer)
                                                           (with-current-buffer buffer
                                                             (and buffer-file-name
                                                                  (equal project-root
                                                                         (textmate-project-root)))))
                                                         ido-temp-list))))
         (ido-make-buffer-list-hook (append ido-make-buffer-list-hook (list filter-fn))))
    (ido-switch-buffer)))

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

(global-set-key (kbd "<C-s-268632066>") 'textmate-plus-list-project-buffers)
(global-set-key (kbd "<C-s-268632080>") 'textmate-plus-switch-to-project)

