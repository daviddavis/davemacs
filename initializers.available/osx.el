;; OS X specific customizations

;; Since it's going to be there anyways... might as well.
(menu-bar-mode 1)
(scroll-bar-mode 1)

(defun osx-zoom-frame ()
  (interactive)
  (set-frame-position (selected-frame) 0 0)
  (set-frame-size (selected-frame) 100 1000))

(defun osx-maximize-frame ()
  (interactive)
  (set-frame-position (selected-frame) 0 0)
  (set-frame-size (selected-frame) 1000 1000))


(defun finder-reveal ()
  "revealed the current file in finder"
  (interactive)
  (if (buffer-file-name)
      (ns-service-Finder-Reveal (buffer-file-name))
    (message "This buffer has no file")))

(defun delete-window-or-frame ()
  (interactive)
  (if (= 1 (count-windows))
      (delete-frame)
    (delete-window)))

(global-set-key (kbd "s-w") 'delete-window-or-frame)
(global-set-key (kbd "s-?") 'help)
(global-set-key (kbd "<s-S-return>") 'osx-maximize-frame)
(global-set-key (kbd "<C-s-268632090>") 'osx-zoom-frame)
