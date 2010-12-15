(add-to-list 'load-path "~/.emacs.d/vendor/vimpulse")
(add-to-list 'load-path "~/.emacs.d/vendor/vimpulse-surround/")

(require 'vimpulse)
(require 'vimpulse-surround)

(viper-mode)

(custom-set-variables
 '(viper-ESC-moves-cursor-back nil)
 '(viper-expert-level '5)
 '(viper-auto-indent nil)

 ;; cursor color changes causes bad lag on OSX, for whatever reason, so disable them
 '(viper-insert-state-cursor-color "")
 '(viper-emacs-state-cursor-color "")
 '(viper-replace-overlay-cursor-color "")

 '(viper-ex-style-editing nil)
 '(viper-ex-style-motion nil)
 '(viper-shift-width 2)
 '(viper-vi-style-in-minibuffer nil))

;; viper is set to default to use viper mode, but many modes come up as emacs that aren't specified to do so
;; thus, the following is necessary:
(dolist (mode '(ack-mode
                dired-mode
                ruby-mode
                feature-mode
                clojure-mode))
  (add-to-list 'viper-vi-state-mode-list mode))

(global-set-key (kbd "s-j") 'viper-intercept-ESC-key) ;; escape is hard to reach... this is easy!

;; This ensures that Emacs will only undo up to the last insert/modal state change.
(add-hook 'viper-insert-state-hook 'undo-boundary t)
(add-hook 'viper-emacs-state-hook 'undo-boundary t)
(add-hook 'viper-vi-state-hook 'undo-boundary t)


;; Cursor magic ------------
;;   This should be default, or there should be a better way to change this... for now, this code
;;   handles changing the cursor from a square to a line
(add-hook 'viper-insert-state-hook 'viper-set-cursor t)
(add-hook 'viper-emacs-state-hook 'viper-set-cursor t)
(add-hook 'viper-vi-state-hook 'viper-set-cursor t)
(defun viper-set-cursor ()
  "Set the cursor for a given state"
  (cond
   ((eq viper-current-state 'emacs-state) (setq cursor-type 'bar))
   ((eq viper-current-state 'vi-state) (setq cursor-type 'hollow))
   ((eq viper-current-state 'viper-mark-state) (setq cursor-type 'bar))
   ((eq viper-current-state 'visual-state) (setq cursor-type 'hbar))))

(defun viper-restore-cursor-type ()
  (viper-set-cursor))


;; HANDS OFF, viper!
;; (define-key viper-vi-basic-map (kbd "C-e") nil)
;; (define-key viper-vi-basic-map (kbd "C-y") nil)
(define-key viper-vi-basic-map (kbd "C-u") 'viper-scroll-down)
(define-key viper-vi-basic-map (kbd "TAB") nil)
(define-key viper-vi-basic-map (kbd "<tab>") nil)
(define-key viper-insert-basic-map (kbd "C-d") nil)
(define-key viper-insert-basic-map (kbd "C-t") nil)
(define-key viper-insert-basic-map (kbd "TAB") nil)
(define-key viper-insert-basic-map (kbd "<tab>") nil)

(define-key viper-insert-basic-map (kbd "DEL") nil)
(define-key viper-insert-basic-map (kbd "C-h") nil)
(define-key viper-insert-basic-map (kbd "C-v") nil)

(defadvice viper-change-state (after viper-change-state-with-key-unfixing activate)
  (define-key viper-insert-basic-map [backspace] nil))

;; Fix o/O to indent according to mode
(defadvice viper-Open-line (after viper-Open-line-with-indentation activate)
  (indent-according-to-mode))

(defadvice viper-open-line (after viper-open-line-with-indentation activate)
  (indent-according-to-mode))

;; emacs keybindings on the terminal has these... since we already have functions that do it, lets create some keybindings!
(global-set-key (kbd "C-]") 'viper-find-char-forward)
(global-set-key (kbd "C-M-]") 'viper-find-char-backward)

;; FIX: the C-i <tab> separator logic breaks things horribly for me
(global-set-key [tab] nil)

;; wdired-mode and viper mode conflict horribly... so lets just make and turn it off right now
(defadvice wdired-change-to-wdired-mode (after wdired-change-to-wdired-mode-emacs-mode activate)
  (viper-change-state-to-emacs))

;; integrate vimpulse better with keybindings for modes
(eval-after-load 'full-ack
  '(progn
     (setq viper-emacs-state-mode-list
           (delq 'ack-mode viper-emacs-state-mode-list))
     (add-to-list 'viper-vi-state-mode-list 'ack-mode)
     (let ((map ack-mode-map))
       (vimpulse-add-core-movement-cmds map)
       (vimpulse-inhibit-destructive-cmds map)
       (viper-modify-major-mode 'ack-mode 'vi-state map))))

