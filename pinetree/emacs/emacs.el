;;==============================================================================
;; Environment setup
;;==============================================================================
;;
(setq pineenv-root    (getenv "PINEENV_ROOT"))
(unless pineenv-root
  (setq pineenv-root (concat (getenv "HOME") "/pineenv")))

;; Get a different file name depending on system-type. Replace '/' with '-',
;; which is used in 'gnu/linux' system-type.
(setq system-type-string
      (replace-regexp-in-string "/" "-" (symbol-name system-type)))

(defun path-in-pine-emacs-root (arg)
  (concat pineenv-root "/emacs/" arg))

(defun pine-add-load-path (str)
  (setq load-path (nconc (list (path-in-pine-emacs-root str)) load-path)))

(pine-add-load-path ".")
(pine-add-load-path "./external")

;; Skip the boring message
(setq inhibit-startup-screen t)

;;==============================================================================
;; Sytem type dependent part(pre)
;;==============================================================================
(require (intern (concat "emacs-sys-" system-type-string)))
(emacs-system-type-dependent-conf 'pre)


;;==============================================================================
;; General Configuration
;;==============================================================================

(setq custom-file (path-in-pine-emacs-root "emacs.custom"))
(load custom-file)

(setenv "EDITOR" "emacsclient")

;; Flash screen instead of noisy bell
(setq visible-bell t)

(global-font-lock-mode t)

;; Show column number 
(column-number-mode t)

;; Show matched parenthesis when the cursor is around it
(show-paren-mode t)

;; Use one letter 'y'/'n' instead of yes/no 
(fset 'yes-or-no-p 'y-or-n-p)

;; Use 80 column for 'fill' commands
(set-default 'fill-column 80)

;; Append my local info directory to default info directory list
(nconc Info-default-directory-list (list (concat pineenv-root "/info")))

;; Use flyspell-mode for the text-mode
(add-hook 'text-mode-hook 'flyspell-mode)
;; Use Aspell prgoram
(setq ispell-program-name "aspell")

;; Use 'abbrev_defs' file to save abbreviations
(setq abbrev-file-name (path-in-pine-emacs-root "abbrev_defs"))
(if (file-exists-p abbrev-file-name)
    (read-abbrev-file abbrev-file-name))
(setq save-abbrevs t)

;; For example, files `/foo/bar/mumble/name' and `/baz/quux/mumble/name' would
;; have the following buffer names: bar/mumble/name quux/mumble/name
(setq uniquify-buffer-name-style 'post-forward)

;; Add additional regexps for matching grep hits
(require 'grep)
(nconc grep-regexp-alist '(("^\\(^[./][-0-9a-zA-Z_./]+$\\)" 1 nil)))


;;
;; Backup file related
;;
(setq
 backup-by-copying t		; don't clobber symlinks
 backup-directory-alist
 '((".*" . "~/.tmpbackup"))	; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)		; use versioned backups


;;==============================================================================
;; MODE SPECIFIC
;;==============================================================================

;; Make it easy to go forward / backward between windows configurations
(winner-mode t)

;; Use ido-family verion of find-file/switch-to-buffer
(ido-mode t)
;; Flexible matching means that if the entered string does not match any item,
;; any item containing the entered characters in the given sequence will match.
(setq ido-enable-flex-matching t)

;; Use Ibuffer
(setq ibuffer-shrink-to-minimum-size t)
(setq ibuffer-always-show-last-buffer nil)
(setq ibuffer-sorting-mode 'recency)
(setq ibuffer-use-header-line t)
(global-set-key "\C-x\C-b" 'ibuffer)

;;
;; dired mode
;;
(require 'dired-x)
(setq dired-recursive-deletes t)
(setq dired-recursive-copies t)
(add-hook 'dired-mode-hook 'dired-omit-mode)

;; Let dired try to guess a default target directory.  if there is a dired
;; buffer displayed in the next window, use its current subdir, instead of the
;; current subdir of this dired buffer.
(setq dired-dwim-target t)

;;
;; recentf mode
;; 
(setq recentf-max-saved-items 512)
(setq recentf-max-menu-items 64)
(global-set-key "\C-xf" 'pine-ido-choose-from-recentf)

;;
;; Show kill ring for the first hit of 'M-y'
;;
(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)
(setq kill-ring-max 1024)

;;
;; Shell & Temrinal
;;

;; Prevent mistakes in comint mode
(setq comint-prompt-read-only t)

(ansi-color-for-comint-mode-on)
(require 'shell-toggle)

;; Use C-x to switch to emacs keys in term-mode
(defun pine-term-mode-hook()
  (term-set-escape-char ?\C-x))
(add-hook 'term-mode-hook 'pine-term-mode-hook)


;;
;; Version control
;;

;; Set only what I am using
(setq vc-handled-backends '(Hg Svn Git))

;; Show vc command message
(setq vc-command-messages t)

;; Do not use short-style
(setq vc-log-short-style '(nil))

;; highlights all characters that cross the 80 character line limit.
(require 'highlight-80+)

(defun use-80-columns()
  (setq fill-column 79)
  (highlight-80+-mode t))

(add-hook 'emacs-lisp-mode-hook 'use-80-columns)

;;
;; Python mode related stuff
(require 'emacs-python)

;;
;; Non-windows(emacs -nw) setting
;;
(unless window-system
  (require 'emacs-nw))


;;==============================================================================
;; General Key bindings
;;==============================================================================

(define-prefix-command 'pine-key-bind)
(global-set-key "\C-q" 'pine-key-bind)
(define-key pine-key-bind (kbd "C-q") 'quoted-insert)
(define-key pine-key-bind (kbd "C-u") 'winner-undo)
(define-key pine-key-bind (kbd "C-r") 'winner-redo)
(define-key pine-key-bind (kbd "C-f") 'windmove-right)
(define-key pine-key-bind (kbd "C-b") 'windmove-left)
(define-key pine-key-bind (kbd "C-p") 'windmove-up)
(define-key pine-key-bind (kbd "C-n") 'windmove-down)
(define-key pine-key-bind "u" (lambda () (interactive) (revert-buffer nil t)))
(define-key pine-key-bind "r" 'replace-string)
(define-key pine-key-bind "q" 'imenu)
(define-key pine-key-bind "p" 'my-python-shell-switch-to-shell)

(global-set-key (kbd "C-x p") 'pine-term)

;; Hide all the windows stuff
(when window-system
  (menu-bar-mode 0)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
)

;;
;; use unified context for diff
;;
(setq diff-switches "-u")

(require 'diffstat)
(add-hook 'diff-mode-hook (lambda () (local-set-key "\C-c\C-l" 'diffstat)))


;;
;; Programming related 
;;
(require 'pine-func)
(require 'emacs-programming)
(require 'emacs-misc)

;;==============================================================================
;; System-type specific part(post)
;;==============================================================================
(emacs-system-type-dependent-conf 'post)



