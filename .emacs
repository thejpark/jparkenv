;; www.emacswiki.org/emacs/EmacsNiftyTricks
;; M-g g : goto line
;; M-x linum-mode : line number
;; M-x hg-line-mode : highlight current line

;; shortcut in case you need many windows
;; M-x windmove-left
;; M-x windmove-up : move upper window
;; C-s M-p shows search history

;; C-x Tab is used to add indent rigidly (regardless of indent mode). It also used for region.

;; how to delete first letter in a line in a region?
;; use C-x r k. Mark the start of the region, go to the end of the line, second column,
;; then press the C-x r k then it will remove the rectangle marked so delete first char.

(global-set-key (kbd "C-c b")  'windmove-left)
(global-set-key (kbd "C-c f") 'windmove-right)
(global-set-key (kbd "C-c p")    'windmove-up)
(global-set-key (kbd "C-c n")  'windmove-down)

(global-set-key (kbd "C-c o")  'ff-find-other-file)

;; add PATH
(setq exec-path (append  exec-path (list "/usr/local/bin" "/User/jpark/bin")))
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin:/User/jpark/bin"))

;; create tags
(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (shell-command
   (format "rm -f %s/TAGS; find %s -type f \\( -iname \*.[chS] -o -iname \*.[ch]pp -o -iname \*.py \\) | xargs etags -a -o %s/TAGS"
	   dir-name dir-name dir-name))
  )

;; use tag tables
;; M-x visit-tags-table

;; ido mode enable to choice in files or directories
(ido-mode t)


;; how to change default compile command?
;; (setq compile-command "nmake")
;; (setq compile-command "scons")
;; how to bind f7 to M-x compile
(global-set-key (kbd "<f7>") 'compile)


(setq project "~/")

(defun set_project (dir-name)
  "Set project directory"
  (interactive "DDirectory: ")
  (setq project
   (format "%s" dir-name))
  (set-frame-name project)
)

(global-set-key (kbd "<f1>") 'set_project)

(defun goto_project_dir ()
  (interactive)
  (dired project))

(global-set-key (kbd "<f2>") 'goto_project_dir)

(defun goto-vc-dir ()
  (interactive)
  (vc-dir project))

(global-set-key (kbd "<f3>") 'goto-vc-dir)


;;(setq prj_bookmarks (list (cons "name" "relative-directory")))
 (setq prj_bookmarks
       (list
 	 (cons "output-dir" "Build/RxDebug")
 	 (cons "integtest" "IntegTests")
 	 (cons "feature-file-dir" "IntegTests/features")
 	 (cons "step-file-dir" "IntegTests/steps")
 	 (cons "phantom" "Phantom/phantom")
 	 (cons "hal-rx" "Hal/Rx")
 	 (cons "hal-integ" "Hal/Integ")
 	 ))

(defun goto-bookmark ()
  (interactive)
  (progn
    (setq keywords
	  (mapcar 'car prj_bookmarks))
    (setq keyword (ido-completing-read "goto:" keywords))
    (setq bookmark (assoc keyword prj_bookmarks))
    (setq file (cdr bookmark))
    (setq file (concat project "/" file))
    (find-file file)))

(global-set-key (kbd "<f5>") 'goto-bookmark)

(defun scons-unit ()
  (interactive)
  (let ((arg (format "cd %s; scons --unit-test-run" project)))
    (compile arg)
    ))

(global-set-key (kbd "<f8>") 'scons-unit)


(defun style-checker ()
  (interactive)
  (let ((arg (format "cd %s; ./Tools/ContinuousIntegration/step_stylecheck.sh" project)))
    (compile arg)
    ))

(global-set-key (kbd "<f9>") 'style-checker)


;; C-x r m for bookmark set. C-x r C-h for help.


;; M-x set-frame-name xxx 
;; sets frame name for emacs windows

;; The setq-default command sets values only in buffers that do not have 
;; their own local values for the variable.

(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq c-basic-offset 4)

;; Treat .h as c++
(setq auto-mode-alist (cons '("\\.h$" . c++-mode) auto-mode-alist))


;;M-x highlight-80+-mode

;;C-h v tab-width shows how to change it
;;(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
;; '(tab-width 4))

;; For cygwin environment
;; The value of “PATH” is used by emacs when you are running a shell in emacs, similar to when you are using a shell in a terminal.
;; The exec-path is used by emacs itself to find programs it needs for its features, such as spell checking, file compression, compiling, grep, diff, etc.
;; The value of “PATH” is used by emacs when you are running a shell in emacs, similar to when you are using a shell in a terminal.
;; The exec-path is used by emacs itself to find programs it needs for its features, such as spell checking, file compression, compiling, grep, diff, etc.
 (if (file-directory-p "c:/cygwin/bin")
	 (setq shell-file-name "C:/cygwin/bin/bash.exe"))
;;      (add-to-list 'exec-path "c:/cygwin/bin"))




;; Some people can do like this for cygwin
;;(when (string-equal system-type "windows-nt")
;;  (setq exec-path
;;'(
;;"C:/Program Files (x86)/Emacs/emacs/bin/"
;;"C:/Program Files (x86)/Emacs/EmacsW32/gnuwin32/bin/"
;;"C:/Windows/system32/"
;;"C:/Windows/"
;;"C:/Windows/System32/Wbem/"
;;"C:/Windows/system32/WindowsPowerShell/v1.0/"
;;)
;; ))

;; exaple of setting env var named PATH
;; (setenv "PATH"
;; 		(concat
;; 		 "C:/cygwin/usr/bin" ";"
;; 		 "C:/cygwin/bin" ";"
;; 		 (getenv "PATH")))


