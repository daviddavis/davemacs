;; TODO - make this a toggle-able minor mode.

(defvar toggle-relative-indent-last-fn nil)
(defun toggle-relative-indent ()
  "Toggles relative indentation"
  (interactive)
  (buffer-local-variables)
  (if (and (equal indent-line-function 'indent-relative) toggle-relative-indent-last-fn)
      (progn
        (setq indent-line-function toggle-relative-indent-last-fn
              toggle-relative-indent-last-fn nil))
    (progn
      (make-local-variable 'toggle-relative-indent-last-fn)
      (setq toggle-relative-indent-last-fn indent-line-function
            indent-line-function 'indent-relative)))
  (message "Using %s" indent-line-function))

(defadvice reindent-then-newline-and-indent (around reindent-then-newline-and-indent-maybe activate)
  (if (equal indent-line-function 'indent-relative)
      (newline-and-indent)
    (progn ad-do-it)))

(global-set-key (kbd "C-c TAB") 'toggle-relative-indent)
(eval-after-load 'sgml-mode
  '(define-key sgml-mode-map (kbd "C-c TAB") 'toggle-relative-indent))
