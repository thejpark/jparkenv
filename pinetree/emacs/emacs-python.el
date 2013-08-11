
;; compilation error for py.test
(setq compilation-error-regexp-alist-alist
	  (assq-delete-all 'py-test compilation-error-regexp-alist-alist))
(nconc compilation-error-regexp-alist-alist
	   '((py-test ".* File \"\\([A-Za-z0-9_/\\.:]+\.py\\)\", line \\([0-9]+\\)" 1 2 nil)))
(nconc compilation-error-regexp-alist '(py-test))

(defun delete-trailing-whitespace-for-python-mode()
  (if (eq major-mode 'python-mode)
      (delete-trailing-whitespace)))
(add-hook 'before-save-hook 'delete-trailing-whitespace-for-python-mode)

(defun my-python-hook()
  (interactive)
  (highlight-lines-matching-regexp "import \\(pdb=\\|pytest\\)")
  (highlight-lines-matching-regexp "\\(pdb\\|pytest\\).set_trace()")
  (use-80-columns)

  (define-key python-mode-map (kbd "C-c l") 'python-indent-shift-left)
  (define-key python-mode-map (kbd "C-c r") 'python-indent-shift-right)
  (flyspell-prog-mode))
(add-hook 'python-mode-hook 'my-python-hook)

;; If global python buffer does not exist or the process is not running, run
;; global python process.
(defun my-python-shell-switch-to-shell()
  (interactive)
  (require 'python)
  (unless (comint-check-proc "*Python*")
	(run-python "python -i" nil))
  (python-shell-switch-to-shell))
	

;; Omit some python related bookkeeping files
(setq dired-omit-files (concat dired-omit-files "\\|__pycache__"))

;; To use C-H S to jump to pydoc info file. It assumes that 'python.info' file
;; exists in the default path (/usr/share/info)
(require 'pydoc-info)

(provide 'emacs-python)



