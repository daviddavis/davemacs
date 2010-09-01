(require 'sgml-mode)

(defun sgml-select-tag ()
  (interactive)
  (sgml-get-context)
  (push-mark (point))
  (sgml-skip-tag-forward 1)
  (setq mark-active t)
  (setq transient-mark-mode t))

(defun sgml-select-inside-tag ()
  (interactive)
  (let ((tag-start (progn (sgml-get-context) (point)))
        (tag-end (progn (sgml-skip-tag-forward 1) (point))))
    (goto-char tag-start)
    (forward-sexp)
    (push-mark (point))

    (goto-char tag-end)
    (backward-sexp)
    (setq mark-active t)
    (setq transient-mark-mode t)))



(define-key sgml-mode-map (kbd "s->") 'sgml-select-inside-tag)
(define-key sgml-mode-map (kbd "s-<") 'sgml-select-tag)
