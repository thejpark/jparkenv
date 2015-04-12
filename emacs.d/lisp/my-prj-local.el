(setq prj_bookmarks (list (cons "name" "relative-directory")))
(setq prj_bookmarks
       (list
 	 (cons "1. source-dir" "src/com/example/criminalintent")
 	 (cons "2. layout-dir" "res/layout")
 	 (cons "3. value-dir" "res/values")
  	 (cons "4. original-source-dir" "../../../../../Downloads/AndroidProgramming/17_FileIo/CriminalIntent/src/com/bignerdranch/android/criminalintent")
  	 (cons "5. original-layout-dir" "../../../../../Downloads/AndroidProgramming/17_FileIo/CriminalIntent/res/layout")
 	 (cons "6. original-value-dir" "../../../../../Downloads/AndroidProgramming/17_FileIo/CriminalIntent/res/values")
 	 ))

(defun goto-prj-bookmark ()
  (interactive)
  (progn
    (setq keywords
	  (mapcar 'car prj_bookmarks))
    (setq keyword (ido-completing-read "goto:" keywords))
    (setq bookmark (assoc keyword prj_bookmarks))
    (setq file (cdr bookmark))
    (setq file (concat project "/" file))
    (find-file file)))


(defun logcat ()
  (interactive)
  (let ((arg (format "cd %s; adb logcat" project)))
    (compile arg)
    ))


(defun build ()
  (interactive)
  (let ((arg (format "cd %s; ant debug" project)))
    (compile arg)
    ))

(defun run ()
  (interactive)
  (let ((arg (format "cd %s; android avd" project)))
    (compile arg)
    ))


(setq prj_commands
       (list
 	 (cons "1. build" 'build)
 	 (cons "2. run" 'run)
 	 (cons "3. logcat" 'logcat)
 	 ))


(defun prj-command ()
  (interactive)
  (progn
    (setq keywords
	  (mapcar 'car prj_commands))
    (setq keyword (ido-completing-read "command:" keywords))
    (setq commands (assoc keyword prj_commands))
    (fset 'command (cdr commands))
    (command)))

(provide 'prj-local)
