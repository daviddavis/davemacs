(defun color-theme-black-pearl-ii ()
  (interactive)
  (color-theme-install
   '(color-theme-black-pearl-ii
     ((background-color . "#000000")
      (background-mode . dark)
      (cursor-color . "#ffffff")
      (foreground-color . "#eaeaea"))

     (default ((t (:inherit nil :stipple nil :background "#000000" :foreground "#eaeaea" :inverse-video nil \.\.\.))))

     (bold ((t (:bold t))))
     (bold-italic ((t (:italic t :bold t))))
     (clojure-test-failure-face ((t (:background "#660000"))))
     (diff-added ((t (:inherit diff-changed :background "green4" :foreground "white"))))
     (diff-refine-change ((((class color) (min-colors 88) (background dark)) (:background "yellow4"))))
     (diff-removed ((t (:inherit nil :background "red3" :foreground "white"))))
     (ecb-tag-header-face ((((class color) (background dark)) (:background "#ffa55f" :foreground "black"))))
     (flymake-errline ((t (:background "LightSalmon" :foreground "black"))))
     (flymake-warnline ((t (:background "LightSteelBlue" :foreground "black"))))
     (font-latex-sectioning-0-face ((t (:inherit font-latex-sectioning-1-face :foreground "yellow" :height 1.3))))
     (font-latex-sectioning-1-face ((t (:inherit font-latex-sectioning-2-face :foreground "#ff77bb" :height 1.3))))
     (font-latex-sectioning-2-face ((t (:inherit font-latex-sectioning-3-face :foreground "#77bbff" :height 1.3))))
     (font-latex-sectioning-3-face ((t (:inherit font-latex-sectioning-4-face :foreground "#77ff77" :height 1.1))))
     (font-lock-builtin-face ((t (:foreground "#3dceff"))))
     (font-lock-builtin-face ((t (:foreground "#aaaF0"))))
     (font-lock-comment-face ((t (:foreground "#2993f3" :italic t))))
     (font-lock-constant-face ((t (:foreground "#3dceff"))))
     (font-lock-doc-string-face ((t (:foreground "#A5C261"))))
     (font-lock-function-name-face ((t (:foreground "#FFC66D" :bold t))))
     (font-lock-keyword-face ((t (:foreground "#CC7833"))))
     (font-lock-preprocessor-face ((t (:foreground "#CC7833"))))
     (font-lock-reference-face ((t (:foreground "LightSteelBlue"))))
     (font-lock-string-face ((t (:foreground "#e356ff"))))
     (font-lock-type-face ((t (:foreground "#82a6c3"))))
     (font-lock-variable-name-face ((t (:foreground "#f5ff9a"))))
     (font-lock-warning-face ((t (:foreground "Pink"))))
     (fringe ((t (:background "#232323"))))
     (highlight ((t (:background "darkolivegreen"))))
     (hl-line ((t (:background "#3f3e00"))))
     (isearch ((t nil)))
     (italic ((t (:italic t))))
     (lazy-highlight ((((class color) (min-colors 88) (background dark)) nil)))
     (magit-item-highlight ((((class color) (background dark)) (:background "#222222"))))
     (minibuffer-prompt ((t (:bold t :foreground "#FF6600"))))
     (mode-line ((t (:background "#ffff77" :foreground "black" :box (:line-width -1 :style released-button)))))
     (mode-line-buffer-id ((t (:foreground "#000000" :background nil))))
     (mode-line-inactive ((t (:foreground "#000000" :background "#777777"))))
     (modeline ((t (:background "#A5BAF1" :foreground "black"))))
     (modeline-buffer-id ((t (:background "#A5BAF1" :foreground "black"))))
     (modeline-mousable ((t (:background "#A5BAF1" :foreground "black"))))
     (modeline-mousable-minor-mode ((t (:background "#A5BAF1" :foreground "black"))))
     (paren-face-match-light ((t (:foreground "#FFC66D" :background "#555577"))))
     (primary-selection ((t (:background "#555577"))))
     (region ((t (:background "#5fa5ff" :foreground "#ffffff"))))
     (secondary-selection ((t (:background "darkslateblue"))))
     (speedbar-directory-face ((((class color) (background dark)) (:foreground "white"))))
     (speedbar-tag-face ((((class color) (background dark)) (:foreground "white"))))
     (textile-h1-face ((t (:foreground "#ffdd77" :underline t :weight bold :height 2.0))))
     (textile-h2-face ((t (:foreground "#66aaff" :underline t :weight bold :height 1.75))))
     (textile-h3-face ((t (:foreground "#66ffaa" :weight bold :height 1.5))))
     (textile-h4-face ((t (:foreground "#ff66aa" :weight bold :height 1.35))))
     (textile-h5-face ((t (:foreground "#ff0066" :underline t :slant italic :weight bold :height 1.2))))
     (textile-h6-face ((t (:foreground "#ff0066" :weight bold :height 1.0))))
     (underline ((t (:underline t))))
     (viper-minibuffer-emacs ((((class color)) nil)))
     (zmacs-region ((t (:background "#555577"))))))
  
  (add-hook 'diff-mode-hook 'diff-mode-fix-stubborn-colors-black-pearl))

(defun diff-mode-fix-stubborn-colors-black-pearl ()
  "diff-mode colors are very stubborn!"
  (interactive)
  (set-face-foreground 'diff-added "white")
  (set-face-foreground 'diff-removed "white"))