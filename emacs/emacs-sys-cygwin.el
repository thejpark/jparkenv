;;==============================================================================
;; Cygwin system type dependent configuration
;;==============================================================================
(setq use-win-svn t)

(defun emacs-system-type-dependent-conf(phase)
  (cond
   ((equal phase 'pre)
    (progn
      ;;
      ;;
      ;; PRE
      ;;
      ;;
      (if use-win-svn
          (setq svn-status-temp-dir "c:/tmp"))


      ))
   ((equal phase 'post)
    (progn
      ;;
      ;;
      ;; POST
      ;;
      ;;
      (when use-win-svn
        ;; To use windows version svn in cygwin for 'psvn'
        (add-hook 'svn-post-process-svn-output-hook 'svn-status-remove-control-M)

        (defun my-remove-control-M ()
          "Remove ^M at end of line in the whole buffer."
          (interactive)
          (let ((buffer-read-only nil))
            (save-match-data
              (save-excursion
                (goto-char (point-min))
                (while (re-search-forward "\r$" (point-max) t)
                  (replace-match "" nil nil))
                ))))
        (defun my-replace-directory-seperator()
          (interactive)
          (let ((buffer-read-only nil))
            (save-match-data
              (save-excursion
                (goto-char (point-min))
                (while (re-search-forward "\\\\" (point-max) t)
                  (replace-match "/" nil nil))

                ))))
        ;; To use windows version svn in cygwin for 'vc'
        (defun vc-svn-remove-control-M(command file-or-list flags)
          (let ((sub-command (nth 0 flags)))
            (when (or (equal command "svn") (equal command "svn-cygwin"))
              (when (member sub-command '("status" "log" "diff" "annotate"))
                (my-remove-control-M))
              (when (member sub-command '("status"))
                (my-replace-directory-seperator))
              )))
        (add-hook 'vc-post-command-functions 'vc-svn-remove-control-M)

        (defadvice vc-svn-command (around vc-svn-command-around nil activate)
          (let ((command (ad-get-arg 3))
                (args (ad-get-args 4)))
            (when (or (and (equal command "status") (member "-v" args))
                      (and (equal command "diff") (not (member "-r" args))))
              (setq vc-svn-program "svn-cygwin"))
            ad-do-it
            (setq vc-svn-program "svn")))
        )
        


      ;; in order to understand Windows path such as c:/abc
      (require 'windows-path)
      (windows-path-activate)

      (require 'gud-cdb)

      (defun cygwin-cut-function (text &optional push)
        (with-temp-buffer
          (insert text)
          (call-process-region (point-min) (point-max) 
                               "bash" nil nil nil "-c" "cat > /dev/clipboard ")))
      (defun cygwin-paste-function()
        (let ((output (shell-command-to-string "cat /dev/clipboard | dos2unix")))
          (unless (string= (car kill-ring) output) output )))
      (setq interprogram-cut-function 'cygwin-cut-function)
      (setq interprogram-paste-function 'cygwin-paste-function)

      (setq py-python-command "python")
      (setq pine-xterm-program "/usr/bin/mintty")
      (setq shell-file-name "bash")

      (defun run-explorer()
        (interactive)
        (shell-command "explorer ."))
      (define-key pine-key-bind "e" 'run-explorer)


      (defun pine-term(arg) 
        "If there is a terminal buffer running, switch to that
buffer and change current working directory of the terminal to
the current directory. Otherwise, a new terminal buffer is
created.

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
      ))
   ))
(provide 'emacs-sys-cygwin)
