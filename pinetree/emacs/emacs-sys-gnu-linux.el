;;==============================================================================
;; GNU/Linux system type dependent configuration
;;==============================================================================
(defun emacs-system-type-dependent-conf(phase)
  (cond
   ((equal phase 'pre)
    (progn
      ;;
      ;;
      ;; PRE
      ;;
      ;;
      (setq default-frame-alist
	    (append '(
		      (font . "-adobe-courier-medium-r-normal--*-100-*-*-m-*-iso8859-1")
		      ;;		    (font . "7x13")        ; small fixed
		      ;;(top . 0)
		      ;;(left . 0)
		      ;;(width . 160)
		      ;;(height . 60)
		      ;;(menu-bar-lines . nil)
		      (tool-bar-lines . nil)
		      ;; (background-color . "Black")
		      ;; (foreground-color . "LightGray")
		      ;; (cursor-color . "Wheat")
		      )))
      (cond
       ((equal system-type 'dgux)
	(progn
	  (load-file "/usr/lib/emacs/lisp/ladebug.el")
	  ))
       ((equal system-type 'gnu/linux)
	(progn
	  (setq Man-switches "-a")
	  ))
       )))
   ((equal phase 'post)
    (progn
      ;;
      ;;
      ;; POST
      ;;
      ;;
      (require 'run-assoc)

      (cond
       ((equal system-type 'darwin)
	(progn
	  (setq associated-program-alist
		'(( "open" ".*")
		  ))))
       (t
	(progn
	  (setq associated-program-alist '(( "kchmviwer" "\\.chm$")
					   ( "okular" "\\.pdf$")
					   ( "freemind" "\\.mm$")
					   ( "djview" "\\.djvu$")
					   ( "oowriter" "\\.\\(doc\\|rtf\\)$")
					   ( "gmplayer" "\\.\\(avi\\|wmv\\|asf\\|asx\\|mp3\\)$")
					   ( "google-chrome" "\\.htm.*$")
					   ))))
       ))))



  (defun pine-term(arg) 
    "If there is a terminal buffer running, switch to that buffer
and change current working directory of the terminal to the
current directory. Otherwise, a new terminal buffer is created.

If `pine-term' is executed inside the terminal buffer, window
configuration is restored to the state right before `pine-term'
was invoked.

prefix can be used to run external xterm with the current
directory as working directory."
    (interactive "P")
    (if arg
	(pine-xterm)
      (setq pine-term-name "pine-term")
      (setq pine-term-buffer (format "*%s*" pine-term-name))
      (setq cmd (format "\C-Ucd %s\n" default-directory))
      (if (eq (current-buffer)(get-buffer pine-term-buffer))
	  (set-window-configuration pine-term-winconf)
	(setq pine-term-winconf (current-window-configuration))
	(if (get-buffer pine-term-buffer)
	    (switch-to-buffer-other-window pine-term-buffer)
	  (ansi-term  "/bin/bash" pine-term-name))
	(term-send-string pine-term-buffer cmd)
	(end-of-buffer))))



  )

(provide 'emacs-sys-gnu-linux)
