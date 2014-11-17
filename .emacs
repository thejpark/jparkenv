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

;; Ctrl-i      Tab
;; Ctrl-[      Esc
;; Ctrl-m      Enter
;; Ctrl-]      jump to tag under cursor
 
;; search keyword under cursor: C-s C-w (keep press C-w for matching words, then C-s again

;; add PATH
(setq exec-path (append  exec-path (list "/usr/local/bin" "/User/jpark/bin")))
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin:/User/jpark/bin"))

;; create tags
(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (shell-command
   (format "rm -f %s/TAGS; find %s -type f  \\( -iname \*.[chS] -o -iname \*.[ch]pp -o -iname \*.py  -o -iname \*.java \\) | xargs etags -a -o %s/TAGS"
	   dir-name dir-name dir-name))
  )

;; use tag tables
;; ido mode enable to choice in files or directories
(ido-mode t)
;; for more information, http://www.emacswiki.org/emacs/InteractivelyDoThings
;; C-s (next), C-r (prev), C-f (fall back to normal mode)

;; how to change default compile command?
;; (setq compile-command "nmake")
;; (setq compile-command "scons")
;; how to bind f7 to M-x compile

(show-paren-mode t)

(setq project "~/")

(defun set-project (dir-name)
  "Set project directory"
  (interactive "DDirectory: ")
  (setq project
   (format "%s" dir-name))
  (set-frame-name project)
  (visit-tags-table project)
)

(defun goto-project-dir ()
  (interactive)
  (dired project))

(defun goto-vc-dir ()
  (interactive)
  (vc-dir project))

;; General bookmarks
(setq gen_bookmarks
       (list
 	 (cons "jparkenv" "jparkenv")
 	 (cons "jparksrc" "wrk/jparksrc")
 	 ))
(defun goto-gen-bookmark ()
  (interactive)
  (progn
    (setq keywords
	  (mapcar 'car gen_bookmarks))
    (setq keyword (ido-completing-read "goto:" keywords))
    (setq bookmark (assoc keyword gen_bookmarks))
    (setq file (cdr bookmark))
    (setq file (concat "~/" file))
    (find-file file)))

(defun find-file-in-project ()
  (interactive)
  (ido-find-file-in-dir project))

(add-to-list 'load-path "~/jparkenv/emacs.d/lisp/")

(require 'browse-kill-ring)
(require 'gtags)

;; evil mode changes
(add-to-list 'load-path "~/jparkenv/emacs.d/evil/") ; only without ELPA/el-get
(require 'evil)
;; (evil-mode 1)
;; (global-set-key (kbd "M-,")  'find-tag)


;; C-_ meanx undo-
;; C-x s means save all

;; C-x r m # for bookmark set. C-x r C-h for help.
;; C-x r b # bookmark jump. C-x r C-h for help.
;; C-x r j # jump to register. C-x r C-h for help.
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

(require 'highlight-80+)

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

;;add this hook as common to all languages 
;; (add-hook 'c-mode-common-hook 'flyspell-prog-mode)
;
;; this is added to remap after flyspell mode, but did not work.
;; (eval-after-load 'flyspell-mode
;;   '(define-key c-mode-map (kbd "<C-;>") 'backward-char))

;; common to all language, but it looks like for c-mode-common-hook I should add-hook only once?
;; (add-hook 'c-mode-common-hook 'jung-c-mode-hook)



(defun my-python-hook()
;;    (interactive)
;;   (highlight-lines-matching-regexp "import \\(pdb=\\|pytest\\)")
;;   (highlight-lines-matching-regexp "\\(pdb\\|pytest\\).set_trace()")
;;   (use-80-columns)

  (define-key python-mode-map (kbd "C-<") 'python-indent-shift-left)
  (define-key python-mode-map (kbd "C->") 'python-indent-shift-right)
;;   (flyspell-prog-mode))
)
(add-hook 'python-mode-hook 'my-python-hook)

; http://blog.jakubarnold.cz/2014/06/23/evil-mode-how-to-switch-from-vim-to-emacs.html
; http://stackoverflow.com/questions/8483182/evil-mode-best-practice
(defun my-evil-mode-hook()
  (define-key evil-normal-state-map (kbd "C-v") 'scroll-up-command)
  (define-key evil-normal-state-map (kbd ",b") 'ido-switch-buffer)
  (define-key evil-normal-state-map (kbd ",,") 'evil-buffer)
  (define-key evil-normal-state-map (kbd ",f") 'ido-find-file)
  (define-key evil-normal-state-map (kbd ",k") 'kill-buffer)
  (define-key evil-normal-state-map (kbd "j") 'next-line)
  (define-key evil-normal-state-map (kbd "k") 'previous-line)
  (define-key evil-normal-state-map (kbd "C-f") 'forward-char)
  (define-key evil-normal-state-map (kbd "C-b") 'backward-char)
  (define-key evil-normal-state-map (kbd "C-n") nil)
  (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
  (define-key evil-normal-state-map (kbd "C-z") 'suspend-frame)
  (define-key evil-insert-state-map (kbd "C-n") nil)
  (define-key evil-normal-state-map (kbd "C-p") nil)
  (define-key evil-insert-state-map (kbd "C-p") nil)
  (define-key evil-normal-state-map (kbd "C-.") nil)
  (define-key evil-normal-state-map (kbd "M-.") nil)
  (define-key evil-normal-state-map (kbd "C-e") nil)
  (define-key evil-insert-state-map (kbd "C-e") nil)
  (define-key evil-motion-state-map (kbd "C-e") nil)
  (define-key evil-normal-state-map (kbd "C-d") nil)
  (define-key evil-insert-state-map (kbd "C-d") nil)
  (define-key evil-insert-state-map (kbd "C-k") nil)
  (define-key evil-insert-state-map (kbd "C-y") nil)
  (define-key evil-insert-state-map (kbd "C-w") nil)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-visual-state-map (kbd "C-c") 'evil-exit-visual-state)
  )

(add-hook 'evil-mode-hook 'my-evil-mode-hook)

;; C-x Tab is used to add indent rigidly (regardless of indent mode). It also used for region.

;; C-x h select entire buffer
;; C-M h select entire function
;; C-u <tab> Shift an entire parenthetical grouping rigidly sideways so that its first line is properly indented.
;; C-M-q Reindent all the lines within one parenthetical grouping.
;; C-M-\ indent region
;; so C-x h C-M-\ make indents for entire buffer

;; M-m -> move to the start in the line (compare with C-a)
;; M-^ -> join line

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


(defun jpark-diff-mode-hook ()
  (define-key diff-mode-map (kbd "M-3") 'save-buffer)
  (define-key diff-mode-map (kbd "M-s") 'other-window)
  (define-key diff-mode-map (kbd "M-2") 'ido-switch-buffer)
  (define-key diff-mode-map (kbd "M-4") 'kill-buffer)
  (define-key diff-mode-map (kbd "s-3") 'save-buffer)
  (define-key diff-mode-map (kbd "s-s") 'other-window)
  (define-key diff-mode-map (kbd "s-2") 'ido-switch-buffer)
  (define-key diff-mode-map (kbd "s-4") 'kill-buffer)
)

(add-hook 'diff-mode-hook 'jpark-diff-mode-hook)


(defun jpark-vc-dir-mode-hook ()
  (define-key vc-dir-mode-map (kbd "M-3") 'save-buffer)
  (define-key vc-dir-mode-map (kbd "M-s") 'other-window)
  (define-key vc-dir-mode-map (kbd "M-2") 'ido-switch-buffer)
  (define-key vc-dir-mode-map (kbd "M-4") 'kill-buffer)
  (define-key vc-dir-mode-map (kbd "s-3") 'save-buffer)
  (define-key vc-dir-mode-map (kbd "s-s") 'other-window)
  (define-key vc-dir-mode-map (kbd "s-2") 'ido-switch-buffer)
  (define-key vc-dir-mode-map (kbd "s-4") 'killbuffer)
  (define-key vc-dir-mode-map (kbd "k") 'previous-line)
  (define-key vc-dir-mode-map (kbd "j") 'next-line)
)

(add-hook 'vc-dir-mode-hook 'jpark-vc-dir-mode-hook)

(defun jpark-dired-mode-hook ()
  (define-key dired-mode-map (kbd "M-3") 'save-buffer)
  (define-key dired-mode-map (kbd "M-s") 'other-window)
  (define-key dired-mode-map (kbd "M-2") 'ido-switch-buffer)
  (define-key dired-mode-map (kbd "M-4") 'kill-buffer)
  (define-key dired-mode-map (kbd "s-3") 'save-buffer)
  (define-key dired-mode-map (kbd "s-s") 'other-window)
  (define-key dired-mode-map (kbd "s-2") 'ido-switch-buffer)
  (define-key dired-mode-map (kbd "s-4") 'kill-buffer)
)

(add-hook 'dired-mode-hook 'jpark-dired-mode-hook)






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
;; C-h f function_name (i.e., ido-find-file) shows help for a function
;; C-h v then enter variable to find


;; find command name
;M-x apropos-command RET then press command name

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
;;(global-set-key (kbd "C-c e") 'thing-copy-line)
(global-set-key (kbd "C-c w") 'thing-copy-word)

(defun my-copy-line ()
  (interactive)
  (progn
    (thing-copy-line)
    (move-beginning-of-line())
  )
)

(defun my-kill-line ()
  (interactive)
  (progn
    (move-beginning-of-line())
    (kill-line)
  )
)

(defun my-open-line ()
  (interactive)
  (progn
    (move-end-of-line())
    (open-line())
  )
)

(defun my-enter ()
  (interactive)
  (progn
    (move-end-of-line())
    (newline-and-indent)
  )
)

(require 'prj-local)

(global-set-key (kbd "<f1>") 'goto-gen-bookmark)
(global-set-key (kbd "<f2>") 'evil-mode)
(global-set-key (kbd "<f3>") 'goto-vc-dir)
(global-set-key (kbd "<f4>") 'goto-project-dir)
(global-set-key (kbd "<f5>") 'prj-command)
(global-set-key (kbd "<f6>") 'goto-prj-bookmark)
(global-set-key (kbd "<f7>") 'find-tag)
(global-set-key (kbd "<f8>") 'rgrep)
(global-set-key (kbd "<f9>") 'execute-extended-command)
(global-set-key (kbd "<f10>") 'compile)
(global-set-key (kbd "<f11>") 'next-error)
(global-set-key (kbd "<f12>") 'previous-error)

(global-set-key (kbd "M-1") 'ido-find-file)
(global-set-key (kbd "M-3") 'save-buffer)
(global-set-key (kbd "M-s") 'other-window)
(global-set-key (kbd "M-2") 'ido-switch-buffer)
(global-set-key (kbd "M-4") 'kill-buffer)
(global-set-key (kbd "M-5") 'vc-diff)
(global-set-key (kbd "s-x") 'execute-extended-command)
(global-set-key (kbd "s-1") 'ido-find-file)
(global-set-key (kbd "s-3") 'save-buffer)
(global-set-key (kbd "s-s") 'other-window)
(global-set-key (kbd "s-2") 'ido-switch-buffer)
(global-set-key (kbd "s-4") 'kill-buffer)

;; check http://ergoemacs.org/emacs/effective_emacs.html for more effective key binding
;; (global-set-key (kbd "M-j") 'backward-char) ; was indent-new-comment-line
;; (global-set-key (kbd "M-l") 'forward-char)  ; was downcase-word
;; (global-set-key (kbd "M-i") 'previous-line) ; was tab-to-tab-stop
;; (global-set-key (kbd "M-k") 'next-line) ; was kill-sentence

;; (global-set-key (kbd "M-[") 'backward-word)
;; (global-set-key (kbd "M-]") 'forward-word) 
(global-set-key (kbd "M-n") 'forward-paragraph) 
(global-set-key (kbd "M-p") 'backward-paragraph) 
;(global-set-key (kbd "C--") 'beginning-of-buffer)
;(global-set-key (kbd "C--") 'other-window)
(global-set-key (kbd "C-;") 'next-line)
(global-set-key (kbd "C-l") 'backward-char)
;(global-set-key (kbd "C-m") 'my-enter)
(global-set-key (kbd "C-o") 'my-open-line)
(global-set-key (kbd "C-'") 'forward-char)
;(global-set-key (kbd "C-=") 'ido-switch-buffer)

(global-set-key (kbd "C-c b")  'windmove-left)
(global-set-key (kbd "C-c f")  'windmove-right)
(global-set-key (kbd "C-c p")  'windmove-up)
(global-set-key (kbd "C-c n")  'windmove-down)

(global-set-key (kbd "C-c o")  'ff-find-other-file)

(global-set-key (kbd "C-c d")  'gtags-find-tag)
(global-set-key (kbd "C-c r")  'gtags-find-rtag)
(global-set-key (kbd "C-c s")  'gtags-find-symbol)

(global-set-key (kbd "C-c v")  'insert-register)
;(global-set-key (kbd "C-c c")  'copy-to-register)
(global-set-key (kbd "C-c c")  'kill-ring-save)

(global-set-key (kbd "C-c l") 'recenter)
(global-set-key (kbd "C-.") 'my-copy-line)
(global-set-key (kbd "C-,") 'my-kill-line)


;; http://www.emacswiki.org/emacs/IndentingText, shift left and right
(defun shift-region (distance)
  (let ((mark (mark)))
    (save-excursion
      (indent-rigidly (region-beginning) (region-end) distance)
      (push-mark mark t t)
      ;; Tell the command loop not to deactivate the mark
      ;; for transient mark mode
      (setq deactivate-mark nil))))

(defun shift-line (distance)
  (let ((mark (mark)))
    (save-excursion
      (indent-rigidly (line-beginning-position) (line-end-position) distance)
      (push-mark mark t t)
      ;; Tell the command loop not to deactivate the mark
      ;; for transient mark mode
      (setq deactivate-mark nil))))

(defun shift-right ()
  (interactive)
  (shift-region 1))

(defun shift-left ()
  (interactive)
  (shift-region -1))

(defun tab-right ()
  (interactive)
  (shift-line 4))

(defun tab-left ()
  (interactive)
  (shift-line -4))

;; Bind (shift-right) and (shift-left) function to your favorite keys. I use
;; the following so that Ctrl-Shift-Right Arrow moves selected text one 
;; column to the right, Ctrl-Shift-Left Arrow moves selected text one
;; column to the left:

(global-set-key [C-S-right] 'shift-right)
(global-set-key [C-S-left] 'shift-left)
(global-set-key (kbd "C->") 'tab-right)
(global-set-key (kbd "C-<") 'tab-left)

;; (defun my-return ()
;;   (interactive)
;;   (move-end-of-line())
;;   (newline-and-indent()))

;; (global-set-key (kbd "C-J") 'my-return)
;; (global-set-key (kbd "C-O") 'my-space)

;; C-x C-+  (or =) for increase font size, C-x C--for decrease
;; C-spc C-spc to mark to remember a position, and C-u C-spc to go back




;;; Shell mode
;; see this http://amitp.blogspot.com.au/2007/04/emacs-color-in-shell-buffers.html
;; also this https://snarfed.org/why_i_dont_run_shells_inside_emacs

(custom-set-variables
 '(comint-scroll-to-bottom-on-input t)  ; always insert at the bottom
 '(comint-scroll-to-bottom-on-output t) ; always add output at the bottom
 '(comint-scroll-show-maximum-output t) ; scroll to show max possible output
 '(comint-completion-autolist t)        ; show completion list when ambiguous
 '(comint-input-ignoredups t)           ; no duplicates in command history
 '(comint-completion-addsuffix t)       ; insert space/slash after file completion
 )
(setq ansi-color-names-vector ; better contrast colors
      ["black" "red4" "green4" "yellow4"
       "blue3" "magenta4" "cyan4" "white"])
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(add-hook 'shell-mode-hook 
     '(lambda () (toggle-truncate-lines 1)))
(setq comint-prompt-read-only t)

;; (global-set-key (kbd "C-?") 'help-command)
;; (global-set-key (kbd "C-h") 'delete-backward-char)
;; (global-set-key (kbd "M-?") 'mark-paragraph)
;; (global-set-key (kbd "M-h") 'backward-kill-word)

;; for vim key bindings
;; http://www.keyxl.com/aaa8263/290/vim-keyboard-shortcuts.htm

;; M-j  means new comment line



;;(setq prj_bookmarks (list (cons "name" "relative-directory")))
;;  (setq prj_bookmarks
;;        (list
;;  	 (cons "output-dir" "Build/RxDebug")
;;  	 (cons "integtest" "IntegTests")
;;  	 (cons "feature-file-dir" "IntegTests/features")
;;  	 (cons "step-file-dir" "IntegTests/steps")
;;  	 (cons "phantom" "Phantom/phantom")
;;  	 (cons "hal-rx" "Hal/Rx")
;;  	 (cons "hal-integ" "Hal/Integ")
;;  	 ))


;; (defun goto-prj-bookmark ()
;;   (interactive)
;;   (progn
;;     (setq keywords
;; 	  (mapcar 'car prj_bookmarks))
;;     (setq keyword (ido-completing-read "goto:" keywords))
;;     (setq bookmark (assoc keyword prj_bookmarks))
;;     (setq file (cdr bookmark))
;;     (setq file (concat project "/" file))
;;     (find-file file)))


;; (defun scons-unit ()
;;   (interactive)
;;   (let ((arg (format "cd %s; scons --unit-test-run" project)))
;;     (compile arg)
;;     ))


;; (defun style-checker ()
;;   (interactive)
;;   (let ((arg (format "cd %s; ./Tools/ContinuousIntegration/step_stylecheck.sh" project)))
;;     (compile arg)
;;     ))


;; (defun scons ()
;;   (interactive)
;;   (let ((arg (format "cd %s; scons" project)))
;;     (compile arg)
;;     ))

;; (setq prj_commands
;;        (list
;;  	 (cons "1. style-checker" 'style-checker)
;;  	 (cons "2. unit-test" 'scons-unit)
;;  	 ))


;; (defun prj-command ()
;;   (interactive)
;;   (progn
;;     (setq keywords
;; 	  (mapcar 'car prj_commands))
;;     (setq keyword (ido-completing-read "command:" keywords))
;;     (setq commands (assoc keyword prj_commands))
;;     (fset 'command (cdr commands))
;;     (command)))

;; (provide 'prj-local)


;; How to make emacs accept UTF-8 from the keyboard
;; http://superuser.com/questions/410100/how-to-make-emacs-accept-utf-8-from-the-keyboard

;; and how to make emacs in cygwin deal with ^M when do vc-dir
;;(setq default-process-coding-system '(undecided-dos . utf-8-unix))
;; search for the default-process-coding-system in the emacs source folder to take a look
;; M-x apropos-command coding-system shows related command
;; M-x describe-current-coding-system shows current coding system
