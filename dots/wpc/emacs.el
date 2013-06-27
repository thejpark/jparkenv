;; Common 
(load "~/pineenv/emacs/emacs.el")

;; Company specific
(load "~/pineenv/dots/wpc/resmed.el")

;; Project specific
(load "~/pineenv/dots/wpc/project-cam.el")

(if (eq system-type 'windows-nt)
    (load "~/pineenv/dots/wpc/emacs-win32.el"))

(add-to-list 'auto-mode-alist '("\\.mak" . makefile-mode))
(setenv "DISPLAY" "localhost:0.0")

;; To use windows version svn in cygwin for 'vc'

;;(setq vc-svn-program "c:/Program Files/WANdisco/Subversion/svn")

(menu-bar-mode 0)
(setq hide-ifdef-shadow t)
(put 'dired-find-alternate-file 'disabled nil)


;; Disable tramp which cause some 'Recursive load error...'
(setq tramp-mode nil)
(setq tramp-unload-tramp)
