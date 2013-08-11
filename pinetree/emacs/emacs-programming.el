
(which-function-mode t)

;;
;; C / C++ Programming
;;
(defun c-find-sexp-forward()
  (interactive)
  (forward-sexp)
  (backward-sexp)
  (mark-sexp)
  (setq sexp (buffer-substring (region-beginning) (region-end)))
  (deactivate-mark)
  (forward-char 1)
  (if (search-forward sexp nil t)
      (backward-sexp)
    (message "no more")
    (backward-char 1))

  )

(defun delete-trailing-whitespace-for-c++-mode()
  (if (eq major-mode 'c++-mode)
      (delete-trailing-whitespace)))
(add-hook 'before-save-hook 'delete-trailing-whitespace-for-c++-mode)


(defun c-find-sexp-backward()
  (interactive)
  (forward-sexp)
  (backward-sexp)
  (mark-sexp)
  (setq sexp (buffer-substring (region-beginning) (region-end)))
  (deactivate-mark)
  (unless (search-backward sexp nil t)
    (message "no more"))
)

(defun pine-c-mode-hook ()
  (local-set-key (kbd "M-p") 'c-find-sexp-backward)
  (local-set-key (kbd "M-n") 'c-find-sexp-forward)

  ;; hide-ifdef
  ;; (hide-ifdef-mode t)
  ;; (modify-face 'hide-ifdef-shadow "gray")
  ;; (setq hide-ifdef-shadow t)

  (if buffer-file-name
      (if (string-match "/linux.*/" buffer-file-name)
	  (progn
	    (c-set-style "linux")
	    (setq
	     tab-width 8
	     indent-tabs-mode t)))))

(add-hook 'c-mode-hook 'pine-c-mode-hook)
(add-hook 'c++-mode-hook 'pine-c-mode-hook)

(add-hook 'c-mode-common-hook 'flyspell-prog-mode)

;; Show trailing whitspace in diff-mode
;;(add-hook 'diff-mode-hook (lambda() (whitespace-mode)))


;;
;;
;; Key bindings
;;
;;
(global-set-key (kbd "C-c s o") 'ff-find-other-file)

(provide 'emacs-programming)
