;;
;; Project specific : Flow generator
;;

;; switch between xml and py for Integ test
(require 'find-file)
(nconc cc-other-file-alist 
       '(("\\.xml$"   (".py" ".cpp"))
         ("\\.py$"   (".xml" ".cpp"))))


;; For xml
(setq nxml-child-indent 4)


;;
;; Find a correct filename from python error
;;
(defun fg-compilation-parse-errors-filename-function (path)
  (if (not tce-current-project)
	  path
	(cond
	 ((string-match "^./.*\\.py" path)
	  (format "%s/Nexus/Build/Integ/Win32/%s" (tce-root) path)
	  )
	 (t path))))

;(setq compilation-parse-errors-filename-function 'fg-compilation-parse-errors-filename-function)

(setq cc-search-directories '("." "./Include" "../Include" "../CortexM3" "../Win32" ".."))


(require 'tce)

(defun tce-fg-compile(&optional arg)
  (let ((options nil))
	(cond
	 ((eq arg 1)
	  (setq fg-compilation 'release)
	  "make -j8 -C Nexus/Build/Release/CortexM3"
	  )
	 ((eq arg 2)
	  (setq fg-compilation 'release)
	  "make -j8 -C Bootloader/Build/Loader"
	  )
	 (t
	  (setq fg-compilation 'integ)
      "make -j8 -C Nexus/Build/Integ/Win32"))))


(defun tce-fg-debug(&optional arg)
  (interactive)
  (let* (
         (cmd "C:/Program Files/Microsoft Visual Studio 8/Common7/IDE/devenv")
         (args (format "ComplianceNotification" (tce-root))))
    (apply 'start-process
           (append (list "run" nil cmd)
                   (split-string args)))))


(defvar fg-debug-history nil)
(defvar tce-debug-args "ComplianceNotification.xml")
(defun tce-fg-debug(&optional arg)
  (interactive "P")
  (cond 
   ((eq arg 2)
    (gud-gdb (format "~/bin/arm-gdb --fullname %s/Bootloader/Build/Loader/Loader.elf" (tce-root))))
   ((and (eq arg 1) (eq system-type 'windows-nt))
    (setq tce-debug-args (read-shell-command "debug args: " tce-debug-args 'fg-debug-history))
    (apply 'start-process
           (append (list "run" nil "python")
                   (list "~/bin/devenv.py" (tce-root))
                   (split-string tce-debug-args)
                   )))
   (t
    (gud-gdb (format "~/bin/arm-gdb --fullname %s/Nexus/Build/Release/CortexM3/Nexus.elf" (tce-root))))
   ))

(defun tce-vc-status(&optional arg)
  (interactive "P")
  (vc-dir (tce-root)))
        
(tce-add-module
 'fg
 (list
  ;; (cons 'switch 'tce-fg-switch)
  (cons 'vc-status 'tce-vc-status)
  (cons 'etag-src-file-name-regexp ".*\\.[chS]$\\|.*\\.[ch]pp$\\|.*\\.py")
  (cons 'compile 'tce-fg-compile)
  (cons 'gdb-func 'tce-fg-debug)
  (cons 'platform "CortexM3")
  (cons 'configuration "Release")
  (cons 'grep-command
	 (lambda (&optional arg)
	   (format (concat "cd %s; find . "
			   "-name \\*.[ch]pp -o -name \\*.[ch] -o -name \\*.mak -o -name [Mm]akefile | xargs /bin/grep -nH ")
		   (tce-root))))
  (cons 'find-command
	 (lambda (&optional arg)
	   (format (concat "cd %s; find . -iname ")
		   (tce-root))))
  (cons 'src-dirs 
	(list
	 "Nexus/Application/Components*"
	 "Nexus/Application/Infrastructure*"
	 "Nexus/Application/Supervisors/ProductSupervisor*"
	 "Nexus/Application/Supervisors/HealthMonitor*"
	 "Nexus/Application/Supervisors/Harness*"
	 "Nexus/Configuration/Integ/NodeDefinitions*"
	 "Nexus/Configuration/Common*"
	 "Nexus/External/Therapy/Include*"
	 "Nexus/External/Therapy/Include*"
         "ExternalSource/ST-Firmware"

         "Nexus/Build/Lib/MdParser"
	 ))
  (cons 'bookmarks
	(list
	 (cons "build" "Nexus/Build/Release/CortexM3")
	 (cons "build-integ" "Nexus/Build/Integ/Win32")
	 (cons "spec" "Nexus/Spec")
	 (cons "Makefiles" "Nexus/Build/Makefiles")
	 (cons "configure.py" "Nexus/Build/Makefiles/configure.py")
	 (cons "mdExt.xml" "Nexus/Spec/mdExt.xml")
	 (cons "nd_cpp.xmp" "Nexus/Configuration/Tools/NodeDefinitions_cpp.xml")
	 (cons "TherapyFiles" "Nexus/Build/Integ/TherapyFiles")
	 ))
    ))

(defun add-project (root)
  (tce-add-project
   (intern root)
   (list
    (cons 'root-dir (format "~/work-misc/%s" root))
    (cons 'module 'fg))))

(mapc 'add-project
      (list
       "main-nexus"
       "nexus_one"
       ))


