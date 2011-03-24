(add-to-list 'load-path "~/.emacs.d/vendor/auto-complete")
(require 'auto-complete-config)

(setq help-xref-following nil) ;; autocomplete causes help to break unless if this is defined.... hmph

(setq ac-use-menu-map t
      ac-auto-show-menu t
      ac-delay 999.0
      ac-expand-on-auto-complete nil
      ac-quick-help-delay 0.25
      ac-use-quick-help t)
(ac-config-default)

(add-to-list 'ac-dictionary-directories "~/.emacs.d/vendor/auto-complete/dict")

(defun auto-complete-or-show-menu ()
  (interactive)
  "Try to autocomplete. If only one possibility, show the menu so we can at least see the help for the method if available"
  (auto-complete)
  (when (not (ac-menu-live-p))
    (progn
      (ac-start :requires t)
      (ac-update-greedy))))

(defun auto-complete-words ()
  (interactive)
  (auto-complete-mode t)
  (let ((ac-sources (list 'ac-source-words-in-all-buffer)))
    (auto-complete)))

(global-set-key (kbd "s-;") 'auto-complete-words)
(global-set-key (kbd "s-:") 'auto-complete-or-show-menu)

(define-key ac-completing-map (kbd "s-;") 'ac-expand)
(define-key ac-completing-map (kbd "ESC") 'ac-stop)
(define-key ac-menu-map (kbd "C-n") 'ac-next)
(define-key ac-menu-map (kbd "C-p") 'ac-previous)
(define-key ac-menu-map (kbd "C-v") 'ac-quick-help-scroll-down)
(define-key ac-menu-map (kbd "M-v") 'ac-quick-help-scroll-up)

(dotimes (i 9)
  (define-key ac-menu-map (number-to-string i) (intern (format "ac-complete-%d" i))))

;; Hack auto-complete to show numbers (for easier selection...)

(defadvice popup-preferred-width (around popup-preferred-width-adjust-for-prefix activate)
  ad-do-it
  (setq ad-return-value (+ 4 ad-return-value)))

(defadvice popup-set-line-item (around popup-set-line-item-with-number activate)
  (let ((item (ad-get-arg 2))
        (item-index (ad-get-arg 3)))
    (ad-set-arg 2
                (concat (format "%2d %s" (+ 1 item-index) item)))
    ad-do-it))
