;;==============================================================================
;; Winddows system type dependent configuration
;;==============================================================================
(require 'eshell)
(defun pine-term-windows(arg) 
  "If there is a terminal buffer running, switch to that buffer
and change current working directory of the terminal to the
current directory. Otherwise, a new terminal buffer is created.

If `pine-term' is executed inside the terminal buffer, window
configuration is restored to the state right before `pine-term'
was invoked.

prefix can be used to run external xterm with the current
directory as working directory."
  (let ((new-dir default-directory)
        (old-eshell-buffer-name eshell-buffer-name))
    (if (or arg (equal mode-name "EShell"))
        ;; if we are arleady eshell mode, open external term
        (pine-xterm)
      (setq eshell-buffer-name (format "*pine-term*"))
      (eshell)
      (setq eshell-buffer-name old-eshell-buffer-name)
      (eshell-kill-input)
      (insert (format "cd \"%s\"" new-dir))
      (eshell-send-input)
      )))

(defun emacs-system-type-dependent-conf(phase)
  (cond
   ((equal phase 'pre)
    (progn
      (setenv "PATH" (concat (getenv "PATH") ";c:/DevTools/Xalan-C_1_10_0-win32-msvc_60/bin;c:/home/hsjang/bin"))
      (setenv "INFOPATH" "c:/Program Files/emacs-23.3/info;c:/cygwin/usr/share/info;c:/home/hsjang/pineenv/info")
      ))

   ((equal phase 'post)
    (progn
      ;;
      ;; post
      ;;

      ;; initial frame is top-left
      (setq default-frame-alist
            '(
              (font . "-outline-Lucida Console-normal-normal-normal-mono-13-*-*-*-c-*-iso10646-1")
              ;;(font . "Bitstream Vera Serif Mono")
              (background-color . "white")	      
              (foreground-color . "black")	      
              (menu-bar-lines . nil)
              (tool-bar-lines . nil)
              ))
      (require 'setup-cygwin)
      (setq grep-use-null-device nil)

      (require 'w32-browser)
      (eval-after-load "dired"
        '(progn
           (define-key dired-mode-map (kbd "M-RET") 'dired-w32-browser)
           (define-key dired-mode-map [C-return] 'dired-w32-browser)
           (define-key dired-mode-map [menu-bar immediate dired-w32-browser]
             '("Open Associated Application" . dired-w32-browser))))

      (defun pine-term(arg) 
        (interactive "P")
        (pine-term-windows arg))

      (define-key pine-key-bind "e" 'w32explore)

      ))
   ))

(provide 'emacs-sys-windows-nt)
