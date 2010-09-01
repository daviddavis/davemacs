(add-to-list 'load-path "~/.emacs.d/vendor/rspec-mode/")

(require 'rspec-mode)

(setq rspec-opts "--drb --color")

(defun rspec-core-options (&optional default-options)
  "Returns string of options that instructs spec to use spec.opts file if it exists, or sensible defaults otherwise"
  (if (file-readable-p (rspec-spec-opts-file))
      (concat "--options " (rspec-spec-opts-file))
    (if default-options
        default-options
      "")))

(defun rspec-format-opts (opts)
  (concat rspec-opts " " (mapconcat (lambda (x) x) opts " ")))

(defun rspec-run (&rest opts)
  "Runs spec with the specified options"
  (rspec-register-verify-redo (cons 'rspec-run opts))
  (compile (concat "rake spec " (concat "SPEC_OPTS=\'" (rspec-format-opts opts) "\'")) t)
  (end-of-buffer-other-window 0))

(defun rspec-run-single-file (spec-file &rest opts)
  "Runs spec with the specified options"
  (rspec-register-verify-redo (cons 'rspec-run-single-file (cons spec-file opts)))
  (compile (concat "rspec " spec-file " " (rspec-format-opts opts)) t)
  (end-of-buffer-other-window 0))

(defun rspec-toggle-spec-and-target-other-window ()
  "rspec-toggle-spec-and-target in other-window"
  (interactive)
  (if (= 1 (count-windows))
      (split-window-horizontally))
  (other-window 1)
  (rspec-toggle-spec-and-target))

(define-key rspec-mode-keymap (kbd "C-c , s-t") 'rspec-toggle-spec-and-target-other-window)

(eval-after-load 'ruby-mode
  '(define-key ruby-mode-map (kbd "C-c , s-t") 'rspec-toggle-spec-and-target-other-window))
