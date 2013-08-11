;;
;; Project specific : Cellular Access Module
;;

(setq vs-devenv-program "c:/Program Files/Microsoft Visual Studio 9.0/Common7/IDE/devenv.exe")
(defun vs-goto-file (&optional arg)
  (interactive "P")
  (message "/edit %s /command \"edit.goto %d\"" (buffer-file-name) (line-number-at-pos))
  (w32-shell-execute "open" vs-devenv-program (format "/edit %s /command \"Edit.GoTo %d\"" (buffer-file-name) (line-number-at-pos))))

(define-key pine-key-bind "v" 'vs-goto-file)


(defun my-interrupt-gdb()
  (interactive)
  (shell-command "ps |grep arm-gdb | cut -d ' ' -f 6 | xargs kill -s 2" ))

(add-hook 'gud-mode-hook (lambda () (local-set-key "\C-c\C-c" 'my-interrupt-gdb)))

(setq auto-mode-alist (cons '("[sS][cC]\\(onscript\\|onstruct\\)" . python-mode) auto-mode-alist))



(require 'tce)

;;
;; tce setup
;;

(defun add-project (root)
  (tce-add-project
   (intern root)
   (list
    (cons 'root-dir (format "~/work/%s" root))
    (cons 'module 'fg))))

(mapc 'add-project
      (list
       "main"
       "main-sim"
       "nexus_one"
       ))
(defvar tce-debug-args "Build/IntegDebug/Cam.exe")

(defun tce-cam-debug(&optional arg)
  (interactive "P")
  (cond
   ((eq arg 1)
    (setq tce-debug-args (read-shell-command "debug args: " tce-debug-args 'fg-debug-history))
    (cdb (format "cdb %s/%s" (tce-root) tce-debug-args)))
   ((eq arg 2)
    (pdb (format "python -m py.test %s/prototypes/CommonArch/Pyhanthom/test_hal.py" (tce-root))))
   ((eq arg 3)
    (gud-gdb (format "~/bin/arm-gdb %s/Build/Stm32Debug/P1.elf" (tce-root))))
   (t
    (setq tce-debug-args (read-shell-command "debug args: " tce-debug-args 'fg-debug-history))
    (apply 'start-process
           (append (list "run" nil "c:/Program Files/Microsoft Visual Studio 9.0/Common7/IDE/devenv.exe")
                   (list (format "%s/%s" (tce-root) tce-debug-args)))))
   ))

(defun tce-cam-compile(&optional arg)
  (interactive "P")
  (cond
   ((eq arg 8)
    (format "cd %s; ./Tools/check_style.py -t python Phantom" (tce-root)))
   ((eq arg 9)
    "./Tools/ContinuousIntegration/step_stylecheck.sh ")
   (t
    "scons"
   )))

(tce-add-module
 'cam
 (list
  (cons 'compile 'tce-cam-compile)
  (cons 'etag-src-file-name-regexp ".*\\.[chS]$\\|.*\\.[ch]pp$\\|.*\\.py")
  (cons 'gdb-func 'tce-cam-debug)
  (cons 'grep-command
	 (lambda (&optional arg)
	   (format (concat "cd %s; find . -name \\*.[ch] -o -name \\*.[ch]pp -o -name Scon\\* | xargs /bin/grep -nH ")
		   (tce-root))))
  (cons 'find-command
	 (lambda (&optional arg)
	   (format (concat "cd %s; find . -iname ")
		   (tce-root))))
  (cons 'src-dirs
	(list
	 "Common*"
	 "EventHorizon*"
	 "Net*"
	 "Fg*"
	 "Hal*"
	 "App*"
	 "External/Serf*"
	 ))
  
  (cons 'user-commands
	(list
	 (cons "gmock-gen" 
		   (lambda ()
			 (shell-command 
			  (format "%s/External/gmock-1.6.0/scripts/generator/gmock_gen.py %s" 
					  (tce-root) (buffer-file-name)) "*Messages*")))
	 (cons "pdb-integ" 
		   (lambda ()
			 (setenv "PYTHONPATH" (format "%s/Tools/Behave:%s/Phantom" 
										  (tce-root) (tce-root)))
			 (cd (format "%s/IntegTests" (tce-root)))
			 (pdb (format 
				   "python -i -m pdb %s/Tools/Behave/behave/main.py --no-capture ." 
				   (tce-root)))))
	 (cons "pdb-pytest" 
		   (lambda ()
			 (pdb (format 
				   "python -i -m pytest %s -s" 
				   buffer-file-truename))))
	 ))
  (cons 'bookmarks
	(list
	 (cons "output-dir" "Build/RxDebug")
	 (cons "integtest" "IntegTests")
	 (cons "feature-file-dir" "IntegTests/features")
	 (cons "step-file-dir" "IntegTests/steps")
	 (cons "phantom" "Phantom/phantom")
	 (cons "hal-rx" "Hal/Rx")
	 (cons "hal-integ" "Hal/Integ")
	 ))

    ))

(tce-add-project
 'main
 (list
  (cons 'root-dir "~/wrk/main")
  (cons 'module 'cam)))

(cd "~")
