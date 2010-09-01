;; ********************************************************************************
;; vines
;; ********************************************************************************

(defun find-non-escaped-paren (kind direction)
  (cond ((= direction -1) (re-search-backward kind))
        ((= direction 1) (search-forward kind) (backward-char))
        (else (error "direction must be -1 or 1"))
        )
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

