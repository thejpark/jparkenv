;;
;; setup for running emacs -nw in X windows or non-X windows.
;;
;; Many things are copied from
;; http://hugoheden.wordpress.com/2009/03/08/copypaste-with-emacs-in-terminal/
;; ======================================================================

;; For mouse-wheel-mode
(require 'mwheel)

(defun setup-for-xenv ()
  (xterm-mouse-mode t)
  (mouse-wheel-mode t)
  (defun xsel-cut-function (text &optional push)
    (with-temp-buffer
      (insert text)
      (call-process-region (point-min) (point-max) "xsel" nil 0 nil "--input")))
  (defun xsel-paste-function()
    ;; Find out what is current selection by xsel. If it is different
    ;; from the top of the kill-ring (car kill-ring), then return
    ;; it. Else, nil is returned, so whatever is in the top of the
    ;; kill-ring will be used.
    (let ((xsel-output (shell-command-to-string "xsel --output")))
      (unless (string= (car kill-ring) xsel-output)
        xsel-output )))
  ;; Attach callbacks to hooks
  (setq interprogram-cut-function 'xsel-cut-function)
  (setq interprogram-paste-function 'xsel-paste-function)

  ;; For the following setting, xterm*VT100.Translations must be set
  (define-key function-key-map "\e[z21" (kbd "C-1"))
  (define-key function-key-map "\e[z22" (kbd "C-2"))
  (define-key function-key-map "\e[z23" (kbd "C-3"))
  (define-key function-key-map "\e[z24" (kbd "C-4"))
  (define-key function-key-map "\e[z25" (kbd "C-5"))
  (define-key function-key-map "\e[z26" (kbd "C-6"))
  (define-key function-key-map "\e[z27" (kbd "C-7"))
  (define-key function-key-map "\e[z28" (kbd "C-8"))
  (define-key function-key-map "\e[z29" (kbd "C-9"))
  (define-key function-key-map "\e[z20" (kbd "C-0"))

  (define-key function-key-map "\e[z3v" (kbd "M-C-S-v"))
  (define-key function-key-map "\e[z3r" (kbd "<C-return>"))
  (define-key function-key-map "\e[z3R" (kbd "<M-return>"))
  (define-key function-key-map "\e[z40" (kbd "<S-left>"))
  (define-key function-key-map "\e[z41" (kbd "<S-right>"))
  (define-key function-key-map "\e[z42" (kbd "<S-up>"))
  (define-key function-key-map "\e[z43" (kbd "<S-down>"))
  (define-key function-key-map "\e[z44" (kbd "<M-S-left>"))
  (define-key function-key-map "\e[z45" (kbd "<M-S-right>"))
  (define-key function-key-map "\e[z46" (kbd "<M-S-up>"))
  (define-key function-key-map "\e[z47" (kbd "<M-S-down>"))
  (define-key function-key-map "\e[z48" (kbd "<M-left>"))
  (define-key function-key-map "\e[z49" (kbd "<M-right>"))
  (define-key function-key-map "\e[z50" (kbd "<M-up>"))
  (define-key function-key-map "\e[z51" (kbd "<M-down>"))
  (define-key function-key-map "\e[z52" (kbd "<C-S-left>"))
  (define-key function-key-map "\e[z53" (kbd "<C-S-right>"))
  (define-key function-key-map "\e[z54" (kbd "<C-S-up>"))
  (define-key function-key-map "\e[z55" (kbd "<C-S-down>")))

(defun setup-for-non-xenv ()
  (xterm-mouse-mode t)
  (mouse-wheel-mode t)

  (defun simple-cut-function (text &optional push)
    (with-temp-buffer
      (insert text)
      (shell-command-on-region 
       (point-min) (point-max) "cat > /tmp/.simple-cut" nil)))

  (defun simple-paste-function()
    (let ((xsel-output (shell-command-to-string "cat /tmp/.simple-cut")))
      (unless (string= (car kill-ring) xsel-output)
	xsel-output )))

  (setq interprogram-cut-function 'simple-cut-function)
  (setq interprogram-paste-function 'simple-paste-function))

(if (and (getenv "DISPLAY") (not (eq system-type 'cygwin)))
    (setup-for-xenv)
  (setup-for-non-xenv))

(defun turn-off-xterm-support()
  (interactive)
    (xterm-mouse-mode nil)
    (mouse-wheel-mode t)
  (setq interprogram-cut-function nil)
  (setq interprogram-paste-function nil))

(provide 'emacs-nw)
