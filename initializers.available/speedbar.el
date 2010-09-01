(autoload 'speedbar "speedbar")
(autoload 'speedbar-add-supported-extension "speedbar")

(setq speedbar-fetch-etags-command "/usr/local/bin/etags"
      speedbar-fetch-etags-arguments '("-f" "-"))
