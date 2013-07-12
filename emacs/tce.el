;;
;; Poor man's IDE.
;;
;; TCE stans for Tagging, compilation and execution.
;;

(require 'etags)
(require 'grep)
(grep-compute-defaults)
(require 'xcscope)

(defgroup tce nil
  "Tagging, Compilation and Running. Poor man's IDE"
  :group 'programming
  :group 'tools)


(defcustom tce-hook nil
  "List of hook functions run by `tce'"
  :type 'hook
  :group 'tce)

(defvar tce-projects nil
  "All the projects configured by `tce-add-project'")

(defvar tce-current-project nil
  "Current project selected among `tce-projects' by `tce-select-project' or '``tce'")

(defun tce-reset-projects()
  (interactive)
  (setq tce-projects nil)
)

(defun tce-attr(attr &optional arg)
  "Return the value associated with `attr'.If it is function and
attr name does not have '-func', it is evaluated recursivelt
until getting final non-function value."
  (let ((result (cdr (assoc attr tce-current-project)))
	module module-symbol)
    ;; check from the current project, If it is 'nil' check in module
    (unless result
      (setq module-symbol (cdr (assoc 'module tce-current-project)))
      (setq module (cdr (assoc module-symbol tce-modules)))
      (setq result (cdr (assoc attr module))))
    ;; If it is still nil, check from default attr
    (unless result
      (setq result (cdr (assoc attr tce-default-project-attr))))
    ;; if the symbol name of 'attr' has '-func', eval the function recursively
    ;; until the return value of function no more function.
    (unless (string-match "-func" (symbol-name attr))
      (while (functionp result)
	(setq result (if arg
			 (funcall result arg)
		       (funcall result)))))
    result))


(defun tce-root()
  "Return the root directory of the current project.
The string is returned after it is expanded."
  (let ((root-dir (tce-attr 'root-dir)))
    (if root-dir
	(expand-file-name root-dir)
      (error "no project selected"))))

(defun tce-root-cygwin()
  (substring (tce-root) 2)) ;; get rid of directory part

(defcustom tce-default-project-attr
  (list
   (cons 'use-etags 1) ; use 1 or 0 instead of t and nil
   (cons 'use-cscope 1)
   (cons 'etag-file-name "TAGS")
   (cons 'etag-src-file-name-regexp ".*\\.[chS]$\\|.*\\.[ch]pp")
   (cons 'cscope-src-file-name-regexp ".*\\.[ch]$")
   (cons 'grep-command grep-command)
   (cons 'use-current-directory-for-compile 0)
   (cons 'dired-home "") ; directory relative from 'root', which is used for `tce-dired'
   (cons 'frame-title
	 (lambda()
	   (tce-attr 'name)))
   (cons 'run-term-func
	 (lambda(command)
	   (apply 'start-process
		  (append (list "run" nil "xterm")
			  (if command (list "-e" "bash" "-c" command))
			  ))))
   ;; list of source directories. (eg. '("abc", "def/ghi")). If the elements can
   ;; end with '*' to include all the children directories.
   (cons 'src-dirs nil)
   ;; It is only used with `src-dirs' attr to exclude directories matching this
   ;; attribute.
   (cons 'src-dirs-exclude-regexp nil)
   (cons 'vc-status-func
	 (lambda(arg)
	   (vc-dir (tce-root))))
   )
  "Alist of default project attribute which is used if user does
not provide that attribute."
  :type 'alist
  :group 'tce)

(defun tce-current-etag-path()
  (setq name (tce-attr 'etag-file-name))
  (concat (tce-root) "/" name))

;;
;; exported funcs
;;
(defun tce-set-project-attr(project-symbol alist)
  (setq project (cdr (assoc project-symbol tce-projects)))
  (dolist (item alist)
    (setq name (car item))
    (setq pair (assoc name project))
    (if pair
	(setcdr pair (cdr item)) ; update value
      (nconc project (list item))))
  (tce-update))


;; exported funcs
;;
(defun tce-set-default-project-attr(alist)
  (dolist (item alist)
    (let ((name (car item)) pair)
      (setq pair (assoc name tce-default-project-attr))
      (if pair
	  (setcdr pair (cdr item)) ; update value
	(message "unknown attr name : %s" (symbol-name name)))))
  (tce-update))


(setq tce-modules nil)
(defun tce-add-module (symbol conf)
  (setq tce-modules (assq-delete-all symbol tce-modules))
  (add-to-list 'tce-modules (cons symbol conf))
  (tce-update))

(setq tce-projects nil)

(defun tce-add-project (symbol conf)
  "Add new tce project.
It is alist of ((module . MODULE) (root-dir . ROOT-DIR))"
  (let* ((rdir (assoc 'root-dir conf))
	 (root-dir (if rdir (cdr rdir))))
    ;; if there is no directory corresponding to 'root-dir, do not add the
    ;; project.
    (if (file-exists-p root-dir)
	(progn
	 (setq tce-projects (assq-delete-all symbol tce-projects))
	 (add-to-list 'conf (cons 'name (symbol-name symbol)))
	 (add-to-list 'tce-projects (cons symbol conf))
	 ;; reflect 'tce-current-project' if the added project is the same as the current
	 ;; project
	 (if (equal (symbol-name symbol) (cdr (assoc 'name tce-current-project)))
	     (setq tce-current-project (cdr (assoc symbol tce-projects))))
	 (tce-update))
      (message "no such directory(%s) for '%s" root-dir (symbol-name symbol))
    )))

(defun tce-config-projects (projects-config)
  (setq tce-projects nil)
  (dolist (project-config projects-config)
    (setq project nil)
    (setq project-symbol (intern (nth 0 project-config)))
    (setq root-dir (nth 1 project-config))
    (setq module (nth 2 project-config))
    (add-to-list 'project (cons 'root-dir root-dir))
    (add-to-list 'project (cons 'module module))
    (tce-add-project project-symbol project)))

(defun tce (&optional project-symbol)
  (interactive)
  (run-hooks 'tce-hook)
  (if (not tce-projects)
      (message "no projects configured")
    (if (interactive-p)
	(let ((project-names
	       (mapcar (lambda (i) (symbol-name (car i))) tce-projects)))
	  (setq project-symbol
		(intern (ido-completing-read "project:" project-names)))))
    (let ((project (cdr (assoc project-symbol tce-projects))))
      (if project (tce-select-project project)
	(message "no such project configured")))))

(defun tce-jump-to-bookmark(&optional keyword)
  "Jump to the file associated `keyword' by looking up
  `bookmarks' attribute of the current project. This function can
  be called interactively to select the keyword"
  (interactive)
  (let (bookmark
	file
	(bookmarks (tce-attr 'bookmarks)))
    (unless bookmarks
      (error "No bookmarks set"))
    (if (interactive-p)
	(progn
	  (setq keywords
		(mapcar 'car (tce-attr 'bookmarks)))
	  (setq keyword (ido-completing-read "jump:" keywords))))
    (setq bookmark (assoc keyword bookmarks))
    (setq file (cdr bookmark))
    (while (functionp file)
      (setq file (funcall file)))
    (unless (string-match "^~\\|^/" file)
      (setq file (concat (tce-root) "/" file)))
    (find-file file)))

(defun tce-run-user-commands(&optional command)
  (interactive)
  (let (bookmark
		file
		(user-commands (tce-attr 'user-commands)))
    (unless user-commands
      (error "No user commands set"))
    (if (interactive-p)
		(progn
		  (setq command-names
				(mapcar 'car (tce-attr 'user-commands)))
		  (setq command-name (ido-completing-read "jump:" command-names))
		  (setq user-command (assoc command-name user-commands))
		  (setq command-str (cdr user-command))
		  (if (functionp command-str)
			  (funcall command-str)
			(shell-command command-str "*Messages*"))
		  ))))

(defun tce-get-sub-dirs(parent-dir &optional dir-pattern max-depth)
  (let ((exclude-regexp (tce-attr 'src-dirs-exclude-regexp)))
    (unless dir-pattern
      (setq dir-pattern "^[^.].*")) ; filter out '.xxx'
    (unless max-depth
      (setq max-depth 255))
    (if (and (file-directory-p parent-dir)
             (not (and exclude-regexp (string-match exclude-regexp parent-dir))))
        (let ((dirs (list parent-dir)) (entries))
          (unless (<= max-depth 1)
            (setq entries (directory-files-and-attributes parent-dir t dir-pattern t))
            (dolist (entry entries)
              (let ((path (nth 0 entry)) (is-dir (equal (nth 1 entry) t)))
                (if is-dir
                    (setq dirs (append dirs (tce-get-sub-dirs path dir-pattern (1- max-depth))))))))
          dirs)
      )))

(defun tce-get-src-dirs()
  "get the final directory list. If the last character is '*',
all the subdirctories are also included"
  (let (result-dirs
	last-char
	(src-dirs (tce-attr 'src-dirs)))
    (dolist (dir src-dirs)
      (setq last-char (substring dir -1))
      (if (string-match "*" last-char)
	  (progn
	    (setq dir (substring dir 0 -1)) ; remove last-char
	    (setq path (concat (tce-root) "/" dir))
	    (if (file-directory-p path)
		(progn
		  (setq result-dirs (append result-dirs (list path)))
		  (dolist (path-sub (tce-get-sub-dirs path nil 255))
		    (setq result-dirs (append result-dirs (list path-sub)))))))
	;; otherwise
	(setq path
	      (if (string-match "^~\\|^/" dir)
		  dir
		(concat (tce-root) "/" dir)))


	(if (file-exists-p path)
	    (setq result-dirs (append result-dirs (list path)))
	  (message "no such file or path : %s" path))))
    result-dirs))

(defun my-shell-command (str)
  "Create a temporary file and copy str into it and run the shell.
This is to work-around the problem in shell-command, which fails
to run 'find' with long list of arguments"
  (let ((fname "/tmp/myshell"))
    (with-temp-file fname 
      (insert (format "%s" str)))
    (shell-command (format "bash %s" fname))
    ))


(defun tce-create-etags ()
  (interactive)
  (let (paths str
	      (output-file-name (tce-current-etag-path))
	      (src-dirs (tce-get-src-dirs))
	      (tag-option (tce-attr 'etag-option)))
    (dolist (path src-dirs)
      (if (file-directory-p path)
	  (setq paths (concat paths path " "))))

    (unless tag-option (setq tag-option ""))
    (setq str
	  (format
	   "rm -f %s; find %s -maxdepth 1 -regex '%s' | xargs ctags -a -e -o %s %s"
           output-file-name
	   paths
	   (tce-attr 'etag-src-file-name-regexp)
	   output-file-name tag-option))
    (message "Creating ETAGS : %s" output-file-name)
    (if (my-shell-command str)
	(progn
	 (message "done")
	 (tce-update))
      (message "FAIL in creating TAGS : %s") output-file-name )
    ))

(defun tce-have-src-files (path)
  "Return t if it has meaningful source files."
  (directory-files path nil (tce-attr 'cscope-src-file-name-regexp) t)
)
(defun tce-set-cscope ()
  "Set `cscope-database-regexps' using `src-dirs', which is the main variable for `xscope.el'."
  (interactive)
  (let (cscope-tree
	(file-name (tce-current-etag-path))
	(src-dirs (tce-get-src-dirs))
	(tag-option (tce-attr 'etag-option)))
    (setq cscope-tree (list ".*"))
    (dolist (path src-dirs)
      (if (and (file-directory-p path) (tce-have-src-files path))
	  (nconc cscope-tree (list (list path)))))
    (nconc cscope-tree '((t) t))
    (setq cscope-database-regexps (list cscope-tree))))

;;(setq project tce-current-project)
(defun tce-select-project(project)
  (unless tce-modules
    (error "No modules set"))
  (setq tce-current-project project)
  (let ((project-name (tce-attr 'name))
	(frame-title (format "%s" (tce-attr 'frame-title)))
	(tag-file (tce-current-etag-path)))

    (if window-system
	(set-frame-name frame-title)
      (if (not (getenv "IN_SCREEN"))
	  ;;(xterm-set-window-title frame-title))
	  (send-string-to-terminal (format "\033]0;%s\a" frame-title))
	(send-string-to-terminal (format "\033k%s\033\\" frame-title)) ; set screen window title
	(send-string-to-terminal (format "\033]0;\a")) ; remove hardstatus
	))


    (tce-update-compilation-command)

    ;;
    ;; cscope
    ;;
    (when (eq (tce-attr 'use-cscope) 1)
      (tce-set-cscope))

    ;;
    ;; etags
    ;;
    (when (eq (tce-attr 'use-etags) 1)
      (tags-reset-tags-tables)
      (when (file-exists-p tag-file)
	(visit-tags-table tag-file)))

    ;; make tce-compile/grep to fetch command string
    (setq tce-compile-command nil)
    (setq tce-grep-command nil)
    (setq tce-find-command nil)
    (tce-attr 'switch)
    ;; execution
    (message "%s selected" project-name)

    ))

(defun tce-update()
  (if (and tce-current-project tce-projects)
      (tce-select-project tce-current-project)))

(defun tce-update-compilation-command (&optional arg)
  "Set tce-compile-command, which is used in compilation"
  (setq tce-compile-command
	(if (eq (tce-attr 'use-current-directory-for-compile) 1)
	    (tce-attr 'compile arg)
	  (format "cd %s; %s" (tce-root) (tce-attr 'compile arg))))
  (setq compile-command tce-compile-command))

(defun tce-compile (&optional arg)
"After project is changed or with prefix, the compilation command is fetched from project attribute.
 Without prefix, it simply calls 'recompile' command. """
 (interactive "P")
 (if (equal arg (list 4))
     (call-interactively 'compile)
   (add-to-list 'compilation-finish-functions 'tce-compilation-finish-function)
   (let ((old-func compilation-buffer-name-function))
     (setq compilation-buffer-name-function
           (lambda (mode-name) 
             (format "*%s-%s*" mode-name (tce-attr 'name))))

     (if (or arg (not tce-compile-command))
         (progn
           (tce-update-compilation-command arg)
           (compile tce-compile-command))
       (recompile))

     (setq compilation-buffer-name-function old-func))))


(defun tce-term(&optional command)
  (interactive)
  (let ((run-term-func (tce-attr 'run-term-func)))
    (funcall run-term-func command)))

(defun tce-execute (&optional arg)
  (interactive "P")
  (let ((command (tce-attr 'run arg)))
    (message "run with :%s" command)
    (if command
	(setq tce-execute-command command)
      (setq tce-execute-command "echo no simulator; sleep 2"))
    (tce-term tce-execute-command)))



(defun tce-compilation-finish-function(buf msg)
  (setq compilation-finish-functions
	(remove 'tce-compilation-finish-function compilation-finish-functions))
  (setq tce-compile-execute-on nil)
  (if (string-match "finished" msg)
      (progn
	(setq after-compile-cmd (tce-attr 'after-compile))
	(if after-compile-cmd
	    (progn
	      (message "run '%s'" after-compile-cmd)
	      (start-process-shell-command "tce-after-compile" "*compilation*" after-compile-cmd)))
	(if tce-compile-execute-on
	    (tce-execute))
	)
    (message "Failed to compile")))

(defun tce-compile-execute()
  "Compile then execute if it succeeds"
  (interactive)
  (setq tce-compile-execute-on t)
  (tce-compile))

(defun tce-term-home(arg)
  (interactive "P")
  (let ((old-dir default-directory))
    (cond 
     ((eq system-type 'windows-nt)
      ;; windows
      (let ((old-eshell-buffer-name eshell-buffer-name))
        (setq eshell-buffer-name (format "*tce-%s*" (tce-attr 'name)))
        (eshell)
        (setq eshell-buffer-name old-eshell-buffer-name)
        (eshell-kill-input)
        (insert (format "cd \"%s\"" (tce-root)))
        (eshell-send-input)))
     (t 
      ;; Linux
      (cd (tce-root))
      (tce-term)
      (cd old-dir)
      (setq tce-term-name "tce-term")
      (setq tce-term-buffer (format "*%s*" tce-term-name))
      (setq cmd (format "\C-Ucd %s\n" (tce-root)))
      (unless (get-buffer tce-term-buffer)
        (ansi-term  "/bin/bash" tce-term-name))
      (end-of-buffer)
      (term-send-string tce-term-buffer cmd)))))


(defun tce-dired-home()
  (interactive)
  (dired (concat (tce-root) "/" (tce-attr 'dired-home))))

(defun tce-gdb(arg)
  "Run GDB. If called with prefix, restart GDB"
  (interactive "P")
  (let ((gdb-func (tce-attr 'gdb-func)))
    (if gdb-func
	(funcall gdb-func arg)
      (message "no gdb-func defined"))))

;;
;; select project depending on current working directory
;;
(defun tce-cwd()
  (interactive)
  (setq project-found tce-current-project)
  (dolist (project tce-projects)
    (setq root-dir (cdr (assoc 'root-dir project)))
    (when (string-match root-dir default-directory)
      (setq project-found project)))
  (tce-select-project project))



(defun tce-grep(&optional arg)
  (interactive "P")
  (let* ((last-command tce-grep-command)
	 (default-command
	   (cond
	    ;; normal prefix or no last-command
	    ((or (numberp arg ) (not last-command))
	     (concat (tce-attr 'grep-command arg) (grep-tag-default)))
	    ;; C-u prefix
	    ((equal arg '(4))
	     last-command)
	    ;; If we can parse the last-command
	    ((string-match "\\(.* -[inH]+ \\)" last-command)
	     (concat (match-string 0 last-command) (grep-tag-default)))
	    ;; if no '-nH' pattern found, just use the same last-command
	    (t
	     last-command)))
	 command)
    (setq command (read-shell-command "grep command: " default-command 'grep-history))
    (setq tce-grep-command command)
    (grep command)))


(defun tce-find(&optional arg)
  (interactive "P")
  (let* ((last-command tce-find-command)
	 (default-command
	   (cond
	    ;; normal prefix or no last-command
	    ((or (numberp arg ) (not last-command))
	     (concat (tce-attr 'find-command arg) (grep-tag-default)))
	    ;; otherwisw, use last-command
	    (t
	     last-command)))
	 command)
    (setq command (read-shell-command "find command: " default-command 'find-history))
    (setq tce-find-command command)
    (grep command)))

(defun tce-tags-search()
  (interactive)
  (let ((regexp (grep-read-regexp)))
    (tags-search regexp)))

(defun tce-tags-apropos()
  (interactive)
  (let ((regexp (grep-read-regexp)))
    (tags-apropos regexp)))

(defun tce-vc-status(&optional arg)
  (interactive "P")
  (let ((vc-status-func (tce-attr 'vc-status-func)))
    (if vc-status-func
        (funcall vc-status-func arg)
      (message "no vc-status-func defined"))))

(define-prefix-command 'tce-key-bind)
(global-set-key (kbd "C-x x ") 'tce-key-bind)
(define-key tce-key-bind "x" 'tce)
(define-key tce-key-bind "X" 'tce-cwd)
(define-key tce-key-bind "c" 'tce-compile)
(define-key tce-key-bind "C" 'tce-compile-execute)
(define-key tce-key-bind "m" 'tce-run-user-commands)
(define-key tce-key-bind "e" 'tce-execute)
(define-key tce-key-bind "h" 'tce-term-home)
(define-key tce-key-bind "d" 'tce-dired-home)
(define-key tce-key-bind "D" 'tce-gdb)
(define-key tce-key-bind "b" 'tce-jump-to-bookmark)
(define-key tce-key-bind "g" 'tce-grep)
(define-key tce-key-bind "f" 'tce-find)
(define-key tce-key-bind "t" 'tce-tags-search)
(define-key tce-key-bind "T" 'tce-tags-apropos)
(define-key tce-key-bind "s" 'tce-vc-status)

(provide 'tce)
