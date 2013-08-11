;; -*- mode: lisp; -*-
;;==============================================================================
;; Minimalist config file 
;;==============================================================================
;;
(setenv "EDITOR" "emacsclient")

(setq visible-bell t)
(global-font-lock-mode t)
(column-number-mode t)
(show-paren-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)
(set-default 'fill-column 80)

;; We don't want TRUNCATED LINES, wrap'em instead
(setq truncate-partial-width-windows nil)

(setq bookmark-default-file "~/.emacs.bmk.mini")

;;==============================================================================
;; MODE SPECIFIC
;;==============================================================================

(ido-mode t)
(setq ido-enable-flex-matching t)
(setq dired-dwim-target t)

;;==============================================================================
;; General Key bindings
;;==============================================================================

(define-prefix-command 'pine-key-bind)
(global-set-key "\C-q" 'pine-key-bind)
(define-key pine-key-bind (kbd "C-u") 'winner-undo)
(define-key pine-key-bind (kbd "C-r") 'winner-redo)
(define-key pine-key-bind (kbd "C-f") 'windmove-right)
(define-key pine-key-bind (kbd "C-b") 'windmove-left)
(define-key pine-key-bind (kbd "C-p") 'windmove-up)
(define-key pine-key-bind (kbd "C-n") 'windmove-down)
(define-key pine-key-bind "q" 'imenu)
(define-key pine-key-bind "u" (lambda () (interactive) (revert-buffer nil t)))
(define-key pine-key-bind "r" 'replace-string)
(define-key pine-key-bind "p" 'run-python)
(define-key pine-key-bind "f" 'select-frame-by-name)
(define-key pine-key-bind "F" 'set-frame-name)
(define-key pine-key-bind "B" 'rename-buffer)

(when window-system
  (menu-bar-mode 0)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
)

;;
;; shell
;;
(ansi-color-for-comint-mode-on)
(setq ispell-program-name "aspell")


;; 
;; Recentf
;;
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

(setq recentf-save-file (convert-standard-filename "~/.recentf-mini"))
(setq recentf-max-saved-items 512)
(setq recentf-max-menu-items 64)
(global-set-key "\C-xf" 'pine-ido-choose-from-recentf)