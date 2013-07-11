;; (load "~/jparkenv/dots/wpc/emacs.el")
;; M-g g : goto line
;; M-x linum-mode : line number

;; shortcut in case you need many windows
;; M-x windmove-left
;; M-x windmove-up : move upper window
(global-set-key (kbd "C-c b")  'windmove-left)
(global-set-key (kbd "C-c f") 'windmove-right)
(global-set-key (kbd "C-c p")    'windmove-up)
(global-set-key (kbd "C-c n")  'windmove-down)

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

;; how to bind f7 to M-x compile
(global-set-key (kbd "<f7>") 'compile)


;; how to change default compile command?
;; (setq compile-command "nmake")

;;
;;(global-set-key (kbd "<f7>") 'foo)
;;(defun foo ()
;;  (let ((arg "cd ~/wrk/test/cpp; g++ ctci.cpp"))
;;    (compile arg)))


(setq project "~/")
(defun set_project (dir-name)
  "Set project directory"
  (interactive "DDirectory: ")
  (setq project
   (format "%s" dir-name)))

(global-set-key (kbd "<f1>") 'set_project)

;; use C-c s o to switch between files (.h for header .vs. .cpp for implementation)


;; C-x r m for bookmark set. C-x r C-h for help.
