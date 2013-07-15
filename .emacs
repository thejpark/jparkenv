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


(setq prj_bookmarks (list (cons "name" "relative-directory")))

;; (setq prj_bookmarks
;;       (list
;; 	 (cons "output-dir" "Build/RxDebug")
;; 	 (cons "integtest" "IntegTests")
;; 	 (cons "feature-file-dir" "IntegTests/features")
;; 	 (cons "step-file-dir" "IntegTests/steps")
;; 	 (cons "phantom" "Phantom/phantom")
;; 	 (cons "hal-rx" "Hal/Rx")
;; 	 (cons "hal-integ" "Hal/Integ")
;; 	 ))

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

;; use C-c s o to switch between files (.h for header .vs. .cpp for implementation)


;; C-x r m for bookmark set. C-x r C-h for help.


;; M-x set-frame-name xxx 
;; sets frame name for emacs windows
