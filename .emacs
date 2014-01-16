;; www.emacswiki.org/emacs/EmacsNiftyTricks
;; M-g g : goto line
;; M-x linum-mode : line number
;; M-x hg-line-mode : highlight current line

;; shortcut in case you need many windows
;; M-x windmove-left
;; M-x windmove-up : move upper window
;; C-s M-p shows search history
;; C-x C-v shows find alternative files with current file as default.

;; how to delete first letter in a line in a region?
;; use C-x r k. Mark the start of the region, go to the end of the line, second column,
;; then press the C-x r k then it will remove the rectangle marked so delete first char.

;; Cancel auto complete in file selection? C-f

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
   (format "rm -f %s/TAGS; find %s -type f \\( -iname \*.[chS] -o -iname \*.[ch]pp -o -iname \*.py \\) | xargs ctags -a -e -o %s/TAGS"
	   dir-name dir-name dir-name))
  )

;; use tag tables
;; M-x visit-tags-table

;; ido mode enable to choice in files or directories
(ido-mode t)
;; for more information, http://www.emacswiki.org/emacs/InteractivelyDoThings
;; C-s (next), C-r (prev), C-f (fall back to normal mode)

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

(defun goto_project_dir ()
  (interactive)
  (dired project))

(global-set-key (kbd "<f4>") 'goto_project_dir)

(defun goto-vc-dir ()
  (interactive)
  (vc-dir project))

(global-set-key (kbd "<f5>") 'goto-vc-dir)


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

(global-set-key (kbd "<f1>") 'ido-find-file)
(global-set-key (kbd "<f2>") 'ido-switch-buffer)
(global-set-key (kbd "<f3>") 'other-window)
(global-set-key (kbd "<f6>") 'goto-bookmark)

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


(add-to-list 'load-path "~/jparkenv/emacs.d/lisp/")
(require 'browse-kill-ring)
;; (require 'okl-style)
(require 'gtags)

(add-to-list 'load-path "~/jparkenv/emacs.d/evil/") ; only without ELPA/el-get
(require 'evil)
(evil-mode 1)
(global-set-key (kbd "M-,")  'find-tag)

(global-set-key (kbd "C-c d")  'gtags-find-tag)
(global-set-key (kbd "C-c r")  'gtags-find-rtag)
(global-set-key (kbd "C-c s")  'gtags-find-symbol)


;; C-_ meanx undo-
;; C-x s means save all

;; C-x r m # for bookmark set. C-x r C-h for help.
;; C-x r j # for bookmark set. C-x r C-h for help.
;; C-x r s # copy register
;; C-x r i # paste

;; C-M k # delete word (so that you can copy and paste)
;; compare with M d

;; M-x set-frame-name xxx 
;; sets frame name for emacs windows

;; The setq-default command sets values only in buffers that do not have 
;; their own local values for the variable.

;; put here makes settings as default for all
;; (setq-default tab-width 4)
;; (setq-default indent-tabs-mode nil)
;; (setq c-basic-offset 4)

;; first non-white space char M-m

(defun use-80-columns()
  (setq fill-column 79)
  (highlight-80+-mode t))


(defun jung-c-mode-hook ()
  (setq c-default-style
	'((java-mode . "java")
	  (c-mode . "bsd")
	  (c++-mode . "bsd")
	  (other . "bsd")
	  ))
  (setq c-basic-offset 4)
  (setq indent-tabs-mode nil)             ;; Use space instead of tab
  (setq tab-width 4)
  (use-80-columns)
  (gtags-mode 1)
  (c-set-offset 'innamespace 0)
  (c-set-offset 'substatement-open 0)
)

(add-hook 'c-mode-hook 'jung-c-mode-hook)
(add-hook 'c++-mode-hook 'jung-c-mode-hook)
(add-hook 'python-mode-hook 'jung-c-mode-hook)

;; add this hook as common to all languages 
(add-hook 'c-mode-common-hook 'flyspell-prog-mode)

;; common to all language, but it looks like for c-mode-common-hook I should add-hook only once?
;; (add-hook 'c-mode-common-hook 'jung-c-mode-hook)



;; (defun my-python-hook()
;;   (interactive)
;;   (highlight-lines-matching-regexp "import \\(pdb=\\|pytest\\)")
;;   (highlight-lines-matching-regexp "\\(pdb\\|pytest\\).set_trace()")
;;   (use-80-columns)

;;   (define-key python-mode-map (kbd "C-c l") 'python-indent-shift-left)
;;   (define-key python-mode-map (kbd "C-c r") 'python-indent-shift-right)
;;   (flyspell-prog-mode))
;; (add-hook 'python-mode-hook 'my-python-hook)


;; C-x Tab is used to add indent rigidly (regardless of indent mode). It also used for region.

;; C-x h select entire buffer
;; C-M h select entire function
;; C-M-\ indent region
;; so C-x h C-M-\ make indents for entire buffer

;; M-m -> move to the start in the line (compare with C-a)

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
;; The value of “PATH” is used by emacs when you are running a shell in emacs, 
;; similar to when you are using a shell in a terminal.
;; The exec-path is used by emacs itself to find programs it needs for its
;; features, such as spell checking, file compression, compiling, grep, diff, etc.
;; The value of “PATH” is used by emacs when you are running a shell in emacs,
;; similar to when you are using a shell in a terminal.
;; The exec-path is used by emacs itself to find programs it needs for its
;; features, such as spell checking, file compression, compiling, grep, diff, etc.

 (if (file-directory-p "c:/cygwin/bin")
	 (progn
	   (setq shell-file-name "bash")
;;  	 (setq shell-file-name "C:/cygwin/bin/bash.exe"))
;;      (add-to-list 'exec-path "c:/cygwin/bin"))

;; This is for ResMed computer.
       (setenv "PATH" (concat (getenv "PATH") ":c:/home/junggyup:c:/home/junggyup/bin"))

))

 (if (file-directory-p "c:/cygwin/bin")
;; if window version of emacs
 	 (if (eq window-system 'w32)
       (setq default-frame-alist
            '(
              (font . "-outline-Lucida Console-normal-normal-normal-mono-13-*-*-*-c-*-iso10646-1")))

))


;;	window-system variable shows which windows system am I using. w32, nil (if used in terminal), etc


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


;; find diff in region
;; (M-x ediff-region-wordwise)


;; emacs word count region
;; Set a region, then press M-=


;; Mode hooks are commonly used to enable minor modes (see Minor Modes). 
;; For example, you can put the following lines in your init file to enable
;;  Flyspell minor mode in all text-based major modes (see Spelling), and
;; Eldoc minor mode in Emacs Lisp mode (see Lisp Doc):
;;     (add-hook 'text-mode-hook 'flyspell-mode)
;;     (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)


(browse-kill-ring-default-keybindings)


;; C-x ( mean macro begin
;; C-x ) means macro end
;; C-x e plays macro
;; M-# or C-u # means repeat command # times


;; M-u uppercase word
;; M-l lowercase word
;; M-c capitalize word
;; C-x C-u uppercase region
;; C-x C-u lowercase region


;;identify what command is bound on the new keys:
;; C-h k then press short cut keys

;; C-h v then enter variable to find


;; find command name
;M-x apropos-command RET then press command name

(global-set-key (kbd "C-c v")  'insert-register)
(global-set-key (kbd "C-c c")  'copy-to-register)

;; C-h m describes current mode


;; M-` activates menu bar


(defun ergoemacs-open-in-desktop ()
  "Show current file in desktop (OS's file manager)."
  (interactive)
  (cond
   ((string-equal system-type "windows-nt")
    (w32-shell-execute "explore" (replace-regexp-in-string "/" "\\" default-directory t t)))
   ((string-equal system-type "darwin") (shell-command "open ."))
   ((string-equal system-type "gnu/linux")
    (let ((process-connection-type nil)) (start-process "" nil "xdg-open" "."))
    ;; (shell-command "xdg-open .") ;; 2013-02-10 this sometimes froze emacs till the folder is closed. ⁖ with nautilus
    ) ))

;; Mac can start application associated with file with 'open' command
;; Windows/Cygwin can start applications with 'cmd /c start' command 


;; This customizing is for terminal version of emacs in Cygwin/Windows.
;; (unless window-system
;;   (custom-set-faces
;;    ;; custom-set-faces was added by Custom.
;;    ;; If you edit it by hand, you could mess it up, so be careful.
;;    ;; Your init file should contain only one such instance.
;;    ;; If there is more than one, they won't work right.
;;    '(flyspell-duplicate ((t (:underline t))))
;;    '(font-lock-constant-face ((t (:foreground "dark cyan"))))
;;    '(font-lock-keyword-face ((t (:foreground "purple" :weight bold))))
;;    '(font-lock-string-face ((t (:foreground "violetRed4"))))
;;    '(font-lock-type-face ((t (:foreground "ForestGreen"))))
;;    '(font-lock-variable-name-face ((t (:foreground "sienna" :weight light)))))
;; )



;; emacs indenting http://www.emacswiki.org/emacs/IndentingC#toc2


(require 'thing-edit)
(global-set-key (kbd "C-c l") 'thing-copy-line)
(global-set-key (kbd "C-c w") 'thing-copy-word)
