(defgroup textile nil
  "textile-mode"
  :prefix "textile-"
  :group 'wp)

(defcustom textile-browser-command '("open")
  "The command invoked to open an html file with a browser.  The first item of the list is the command, the rest are arguments, if any"
  :group 'textile
  :type '(repeat (string)))

(defvar *textile-tmp-html-output-file* "/tmp/emacs-textile.html")

(defun textile-filter-string-through-command (str command &rest ARGS)
  "filters a string through a command, and returns the output"
  (with-temp-buffer
    (insert str)
    (apply 'call-process-region (point-min) (point-max) command t t nil ARGS)
    (filter-buffer-substring (point-min) (point-max))))

(defun textile-render-string (contents &optional target)
  "renders a textile string to a string"
  (textile-filter-string-through-command
   contents
   "ruby"
   "-e"
   "def e(msg)
      puts msg
      exit 1
    end
    begin
      require 'rubygems'
      rescue LoadError
      e('rubygems not found')
    end
    begin
      require 'redcloth'
    rescue LoadError
      e('RedCloth gem not installed.  Run this from the terminal: sudo gem install RedCloth')
    end
    puts(RedCloth.new($stdin.read).to_html(:textile))"))

(defun textile-render-preview (beg end)
  "renders the contents of the current buffer and opens it in your default browser"
  (interactive "r")
  (unless mark-active (setq beg (point-min)
                            end (point-max)))

  (let* ((input (filter-buffer-substring beg end))
         (output (textile-render-string input))
         (command (first textile-browser-command))
         (args (append (rest textile-browser-command) (list *textile-tmp-html-output-file*))))
    (with-temp-buffer
      (insert output)
      (write-file *textile-tmp-html-output-file*))
    (apply 'call-process command nil nil t args)))

(defun textile-render-new-buffer (beg end)
  "renders the contents of the current buffer to a new buffer"
  (interactive "r")
  (unless mark-active (setq beg (point-min)
                            end (point-max)))

  (let* ((input (filter-buffer-substring beg end))
         (new-buffer-name (concat (replace-regexp-in-string "\.textile$" "" (buffer-name)) ".html"))
         (buffer (generate-new-buffer new-buffer-name)))
    (switch-to-buffer buffer)
    (insert (textile-render-string input))))


(eval-after-load 'textile-mode
  '(progn
     (define-key textile-mode-map (kbd "C-c p") 'textile-render-preview)
     (define-key textile-mode-map (kbd "C-c b") 'textile-render-new-buffer)))
