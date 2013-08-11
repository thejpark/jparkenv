;;
;; add timestamp prefix to term-log-buffer
;;
;; Use 'term-timestamp-log-toggle'
;;
;; Author : Hans Jang, 16 Oct 2009.

(defun term-timestamp-output(line)
  (let ((current-time (float-time)) time-line)
    (setq time-line 
          (format "%5.1f|%5.1f : %s\n"
                  (- current-time term-timestamp-base-time)
                  (- current-time term-timestamp-last-time)
                  line))
    (save-excursion
      (set-buffer (get-buffer-create "*timestamp-term-log*"))
      (end-of-buffer)
      (princ time-line (get-buffer-create "*timestamp-term-log*")))
    (setq term-timestamp-last-time current-time)))


(defun term-timestamp-func(char)
  (unless (or (equal char ?\r) (equal char ?\n))
    (setq term-timestamp-line (concat term-timestamp-line (string char))))
  (when (equal char ?\n)
    (term-timestamp-output term-timestamp-line)
    (setq term-timestamp-line nil)))

(defun term-timestamp-log-toggle()
  (interactive)
  (if (equal term-log-buffer 'term-timestamp-func)
      (setq  term-log-buffer 'nil)
    (term-timestamp-reset)
    (setq term-log-buffer 'term-timestamp-func)))

(defun term-timestamp-reset()
  (interactive)
  (setq term-timestamp-base-time (float-time))
  (setq term-timestamp-last-time (float-time))
  (setq term-timestamp-line nil))

(provide 'term-timestamp-log)
