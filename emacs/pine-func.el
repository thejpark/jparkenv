;;
;; Small utility functions
;;

(defvar pine-xterm-program "xterm")
(defvar pine-xterm-args "")

(defun pine-xterm-linux(&optional command xterm-args)
  (interactive)
  (if pine-xterm-args (setq xterm-args (concat xterm-args pine-xterm-args)))
  (cond 
   ((string-match "konsole" pine-xterm-program)
    (if command
	(setq xterm-args 
	      (concat xterm-args " -notabbar -noframe -noscrollbar")))
    ))
  (apply 'start-process
	 (append (list "run" nil pine-xterm-program)
		 (if xterm-args (split-string xterm-args))
		 (if command (list "-e" "bash" "-c" command))
		 )))

(defun pine-xterm-putty(&optional command xterm-args)
  (interactive)
  (let ((prev-lang (getenv "LANG"))
        (cmd "c:/Program Files/puttycyg/putty.exe"))
    (setenv "LANG" "C")
    (apply 'start-process
           (append (list "run" nil pine-xterm-program "-cygterm" "bash")
                   (if command (list "-c" command))))
    (setenv "LANG" prev-lang)
    ))

(defun pine-xterm(&optional command xterm-args)
  (interactive)
  (if (eq system-type 'windows-nt)
      (pine-xterm-putty command)
    (pine-xterm-linux command xterm-args)))
      
    
(defun pine-ido-choose-from-recentf (arg)
  "Use ido to select a recently opened file from the `recentf-list'.
If a prefix is used, the original `recentf' is invoked"
  (interactive "P")
  (unless recentf-mode (recentf-mode))
  (let ((home (expand-file-name (getenv "HOME"))))
    (print arg)
    (if arg
	(recentf-open-files)
      (find-file
       (ido-completing-read "Recentf : "
			    (mapcar (lambda (path)
				      (replace-regexp-in-string home "~" path))
				    recentf-list)
			    nil t)))))

(provide 'pine-func)