(load-file "~/.emacs.d/vendor/textmate/textmate.el")
(require 'textmate)
(textmate-mode t)

(defun cr-after-line (&optional stuff)
  "Insert a new line after the current line (c-a enter)"
  (interactive)
  (end-of-line)
  (reindent-then-newline-and-indent))

(defun cr-before-line (&optional stuff)
  "Insert a new line after the current line (c-p c-a enter)"
  (interactive)
  (indent-according-to-mode)
  (if (= (line-number-at-pos) 1)
      (progn
        (beginning-of-line)
        (newline)
        (previous-line 1))
    (progn
      (previous-line 1)
      (end-of-line)
      (newline-and-indent))))

(defun join-line-textmate-style ()
  (interactive)
  (next-line 1)
  (join-line))

(defun select-outer-paren ()
  (interactive)
  (up-list)
  (setq p1 (point))
  (backward-list)
  (select-points p1 (point)))

(defun select-points (p1 p2)
  (goto-char p1)
  (push-mark p2)
  (setq mark-active t)
  (setq transient-mark-mode t))

;;; Sane search functionality
;; It makes no sense to me whatsoever why the current say search behaves is desirable (s-e appending on to the end of the current search string is just useless... I know there's gotta be a reason for it... but why?)
(defvar query-replace-defaults '("" . ""))

(defun isearch-string-save (&optional p1 p2)
  "Copy the current selection to the search buffer"
  (interactive "*r")
  (setq text (buffer-substring p1 p2)
        isearch-string  (regexp-quote text)
        isearch-message (mapconcat 'isearch-text-char-description isearch-string "")
        query-replace-defaults (cons text (cdr query-replace-defaults)))
  (setq mark-active nil))

(defun replace-string-save (&optional p1 p2)
  "Copy the current selection to the search buffer"
  (interactive "*r")
  (setq text (buffer-substring p1 p2)
        query-replace-defaults (cons (car query-replace-defaults) text))
  (setq mark-active nil))

(let ((set-key (lambda (key function) (define-key *textmate-mode-map* key function))))
  (funcall set-key (kbd "<M-s-return>") 'cr-before-line))

(global-set-key (kbd "s-e") 'isearch-string-save)
(global-set-key (kbd "s-E") 'replace-string-save)
(global-set-key (kbd "s-g") 'isearch-repeat-forward)
(global-set-key (kbd "s-G") 'isearch-repeat-backward)
(global-set-key (kbd "<M-s-return>") 'cr-before-line)

(global-set-key (kbd "<s-return>") 'cr-after-line)
(global-set-key (kbd "C-S-j") 'join-line-textmate-style)
(global-set-key (kbd "C-S-k") 'kill-whole-lines)
(global-set-key (kbd "C-S-d") 'duplicate-line-or-selection)
(global-set-key (kbd "M-s-å") 'append-to-lines) ;; M-s-a
(global-set-key (kbd "s-B") 'select-outer-paren)

(defun kill-whole-lines (&optional arg)
  "like kill-whole-line, but kills everything selected"
  (interactive "P")
  (if mark-active
      (progn (textmate-select-line)
             (filter-buffer-substring (region-beginning) (region-end) t))
    (kill-whole-line arg)))

(defun duplicate-line-or-selection ()
  "duplicate the whole line or just the selection"
  (interactive)
  (if mark-active
      (progn (deactivate-mark)
             (cua-copy-region nil)
             (cua-paste nil))
    (progn (kill-whole-line)
           (cua-paste nil) (cua-paste nil)
           (previous-line)
           (end-of-visual-line))))

(defun append-to-lines (text-to-be-inserted)
  ;;Appends text to each line in region
  (interactive "sEnter text to append: ")
  (save-excursion
    (let (point-ln mark-ln initial-ln final-ln count)
      (barf-if-buffer-read-only)
      (setq point-ln (line-number-at-pos))
      (exchange-point-and-mark)
      (setq mark-ln (line-number-at-pos))
      (if (< point-ln mark-ln)
          (progn (setq initial-ln point-ln final-ln mark-ln)
                 (exchange-point-and-mark))
        (setq initial-ln mark-ln final-ln point-ln))
      (setq count initial-ln)
      (while (<= count final-ln)
        (progn (move-end-of-line 1)
               (insert text-to-be-inserted)
               (next-line)
               (setq count (1+ count))))
      (message "From line %d to line %d." initial-ln final-ln ))))

;; (defadvice paredit-mode (around paredit-mode-turn-off-meta-up-down)
;;   (define-key paredit-mode-map (kbd "M-<down>") nil)
;;   (define-key paredit-mode-map (kbd "M-<up>") nil))
;; (ad-activate 'paredit-mode)

(defun textmate-delete-to-beginning-of-line ()
  "Delete to the beginning of the line"
  (interactive)
  (let ((p1 (point)))
    (beginning-of-line)
    (filter-buffer-substring p1 (point) t)))

(define-key *textmate-mode-map* (kbd "s-<backspace>") 'textmate-delete-to-beginning-of-line)

;; (search-forward-regexp "\\(^.\\{0\\}\\([a-zA-Z0-9_].\\|.[^a-zA-Z0-9_]\\)\\|^.\\{0,1\\}$\\)")

(require 'paredit)
(define-key paredit-mode-map (kbd "M-<down>") nil)
(define-key paredit-mode-map (kbd "M-<up>") nil)


(define-key *textmate-mode-map* (kbd "C-c t k") 'textmate-column-up)
(define-key *textmate-mode-map* (kbd "C-c t j") 'textmate-column-down)


;; (define-key *textmate-mode-map* [(meta <up>)] 'textmate-column-up)


(defun textmate-plus-partial-matching-regex (filename)
  (let* ((pieces (split-string (regexp-quote filename) "/"))
         (path-pieces (subseq pieces 0 (- (length pieces) 1)))
         (filename-piece (last pieces))
         (path-prefix (if (> (length path-pieces) 0)
                          (concat (mapconcat (lambda (i) i) path-pieces "/") "/")
                        "\\(^\\|\\/\\)")))
    (concat path-prefix
            "_?"
            (car filename-piece)
            "\\($\\|\\.[^/]+$\\)")))


(defun textmate-plus-quick-find-file ()
  (interactive)
  (save-excursion
    (let* ((root (textmate-project-root))
           (filename (replace-regexp-in-string "^/*\\.*[/:]" "" (thing-at-point 'filename)))
           (regex (textmate-plus-partial-matching-regex filename))
           (choices (remove-if-not (lambda (item) (string-match regex item)) (textmate-cached-project-files root)))
           (selected-file (cond ((= 1 (length choices)) (car choices))
                                ((= 0 (length choices)) (message (format "no results for '%s'" filename)) nil)
                                (t (textmate-completing-read "Find file: " choices)))))
      (and selected-file (find-file
                          (concat
                           (expand-file-name root) "/"
                           selected-file))))))

(global-set-key (kbd "C-s-t") 'textmate-plus-quick-find-file)
(global-set-key (kbd "<C-s-268632084>") 'textmate-plus-quick-find-file)

(global-set-key (kbd "M-s-†") 'textmate-clear-cache)

(defun kill-beginning-of-line ()
  (interactive)
  (setq p1 (point))
  (beginning-of-line)
  (setq p2 (point))
  (kill-region p1 p2))
(global-set-key (kbd "A-<backspace>") 'kill-beginning-of-line)

(global-set-key (kbd "<s-down>") 'end-of-buffer)
(global-set-key (kbd "<s-up>") 'beginning-of-buffer)
(global-set-key (kbd "<s-left>") 'beginning-of-line)
(global-set-key (kbd "<s-right>") 'end-of-line)

