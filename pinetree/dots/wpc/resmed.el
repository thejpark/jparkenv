(defun resmed-c-mode-hook ()
  (setq c-default-style
	'((java-mode . "java")
	  (c-mode . "bsd")
	  (c++-mode . "bsd")
	  (other . "bsd")
	  ))
  (setq 
   c-basic-offset 4
   indent-tabs-mode nil             ;; Use space instead of tab
   )
  (use-80-columns)
  (c-set-offset 'innamespace 0)
)

(setq-default tab-width 4)

(add-hook 'c-mode-hook 'resmed-c-mode-hook)
(add-hook 'c++-mode-hook 'resmed-c-mode-hook)
(add-hook 'python-mode-hook 'resmed-c-mode-hook)

;; Treat .h as c++
(setq auto-mode-alist (cons '("\\.h$" . c++-mode) auto-mode-alist))

;; Use external diff in vc-svn-diff
;;(setq vc-svn-diff-switches (list "--diff-cmd=c:/gnuwin32/bin/diff" "-x" "-p -u"))


;;
;; Compilation error handlings
;;
(nconc compilation-error-regexp-alist '(msvc iar rx))

;; For MSVC
(setq compilation-error-regexp-alist-alist
      (assq-delete-all 'msvc compilation-error-regexp-alist-alist))
(nconc compilation-error-regexp-alist-alist
       ;; Microsoft C/C++:
       ;;  keyboard.c(537) : warning C4005: 'min' : macro redefinition
       ;;  d:\tmp\test.c(23) : error C2143: syntax error : missing ';' before 'if'
       ;;  .\cppcli1.cpp(36): error C2059: syntax error : 'public'
       ;;  e:\projects\myce40\tok.h(85) : error C2236: unexpected 'class' '$S1'
       ;;  myc.cpp(14) : error C3149: 'class System::String' : illegal use of managed type 'String'; did you forget a '*'?
       '((msvc "^\\([A-Za-z0-9_+/\\.:]*\\.\\(cpp\\|c\\|h\\)\\)(\\([0-9]+\\)) *: +\\(error\\|fatal error\\|warning\\)" 1 3)))

;; For IAR
(setq compilation-error-regexp-alist-alist
      (assq-delete-all 'iar compilation-error-regexp-alist-alist))
(nconc compilation-error-regexp-alist-alist 
       '((iar "^\"\\(.*\\)\",\\([0-9]+\\)\\s-+\\(?:Error\\|Warnin\\(g\\)\\)\\[..[0-9]+\\]:" 1 2 nil (3))))


;; For RX
(setq compilation-error-regexp-alist-alist
      (assq-delete-all 'rx compilation-error-regexp-alist-alist))
(nconc compilation-error-regexp-alist-alist
       ;; Net\NetManager.cpp(22) : C5020 (E) Identifier "GetUartIndex" is undefined
       ;; Net\NetManager.cpp(111) : C5550 (W) Parameter "buff" was set but never used
       '((rx "^\\([A-Za-z0-9_+/\\.:]*\\.\\(cpp\\|c\\|h\\)\\)(\\([0-9]+\\)) : C[0-9]+ (\\(E\\|W\\|F\\))" 1 3)))
