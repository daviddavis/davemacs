(defvar cave-paint-level 0)

(defmacro cave-paint (&rest body)
  ""
  `(progn
     (message "%d : code - %S" cave-paint-level ',@body)
     (setq cave-paint-level (1+ cave-paint-level))
     (let ((cave-paint-value (progn ,@body)))
       (setq cave-paint-level (1- cave-paint-level))
       (message "%d : result - %S" cave-paint-level cave-paint-value)
       cave-paint-value)))
