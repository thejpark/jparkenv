;; -*- lisp -*-
;;
;; GNUPlot
;;
(setq calc-gnuplot-name "c:/Program Files/gnuplot/binary/gnuplot.exe")
(setq gnuplot-program "c:/Program Files/gnuplot/binary/gnuplot.exe")
(setq gnuplot-program-arg "-noend")

(setq wgnuplot-program "c:/Program Files/gnuplot/binary/wgnuplot.exe")
(defun run-gnuplot(&optional arg)
  (interactive "P")
  (if arg
      (w32-shell-execute "open" wgnuplot-program (format "-p %s" (buffer-file-name)))
    (w32-shell-execute "open" wgnuplot-program)))

(define-key pine-key-bind "g" 'run-gnuplot)


;; (setq py-python-command "python2.6")
;; (setq python-python-command py-python-command)
;;(setq pine-xterm-program "c:/Program Files/puttycyg/putty.exe")
(setq pine-xterm-program "c:/cygwin/bin/mintty.exe")
(setq gud-pdb-command-name "python -i -m pdb")
;;(setq vc-svn-program "c:/Program Files/WANdisco/Subversion/svn")


;; To use windows version svn in cygwin for 'vc'
(defun vc-svn-remove-control-M(command file-or-list flags)
  (let ((sub-command (nth 0 flags)))
    (when (string-match ".*svn$" command)
      (my-remove-control-M))
      ))


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
(add-hook 'vc-post-command-functions 'vc-svn-remove-control-M)
