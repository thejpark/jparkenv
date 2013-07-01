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

;; use C-c s o to switch between files (.h for header .vs. .cpp for implementation)

