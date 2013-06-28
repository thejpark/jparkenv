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



