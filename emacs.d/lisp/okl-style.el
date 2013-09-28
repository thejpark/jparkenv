;;
;; CC-mode configuration for the OKL coding style.
;;
;;

(require 'cc-mode)



(defun c-lineup-arglist-okl(ignored)
  (interactive)
  (save-excursion
    (let ((anchor)
	  last-anchor indent)
      (while
	  (setq anchor ((lambda (match)
			  "return anchor pos if current syntatic symbol is `match'"
			  (let* ((syntax (c-guess-basic-syntax))
				 (len (length syntax)))
			    (if (equal (car (nth (1- len) syntax)) match)
				(car (cdr (nth (1- len) syntax)))
			      nil)
			    ))
			'arglist-cont-nonempty))
	(goto-char anchor)
	(setq last-anchor (point)))

      (if last-anchor
	  (progn
	    (goto-char last-anchor)
	    (setq indent (c-get-syntactic-indentation (c-guess-basic-syntax)))
	    (vector (+ indent (* 2 c-basic-offset))))
	nil))))

(defconst ertos-c-style ; based on the BSD style
  '((c-basic-offset . 4)
     (c-comment-only-line-offset . 0)
     (c-cleanup-list . (scope-operator brace-else-brace brace-elseif-brace defun-close-semi list-close-comma))
     (c-offsets-alist . ((statement-block-intro . +)
                         (knr-argdecl-intro . +)
                         (substatement-open . 0)
                         (substatement-label . 0)
                         (label . 0)
                         (inline-open . 0)
                         (inexpr-class . 0)
                         ;; for double indenting of wrapped line 
                         (arglist-intro . ++) 
                         (arglist-cont . 0) 
                         (arglist-cont-nonempty . c-lineup-arglist-okl)
			 (statement-cont . ++)
			 (cpp-define-intro . ++)
                         ))
     )
  "OKL Coding Style.  See the document 10004:2006 rev 2")


;; Register the local styles.
(c-add-style "OKL" ertos-c-style nil)


;;
;; For linux kernel
;;
(defun c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces"
  (let* ((anchor (c-langelem-pos c-syntactic-element))
	 (column (c-langelem-2nd-pos c-syntactic-element))
	 (offset (- (1+ column) anchor))
	 (steps (floor offset c-basic-offset)))
    (* (max steps 1)
       c-basic-offset)))

;; Add Linux kernel style
(c-add-style
 "linux-tabs-only"
 '("linux" (c-offsets-alist
            (arglist-cont-nonempty
             c-lineup-gcc-asm-reg
             c-lineup-arglist-tabs-only))))

;; Customizations for all modes in CC Mode.
(defun okl-c-mode-common-hook ()
  ;; Set the appropriate style for the buffer.
  ;; Use the Linux kernel style for wombat and the default style for everything else.
  (setq show-trailing-whitespace t) 
  (if (and buffer-file-name
             (or (string-match "/linux-.*/" buffer-file-name) 
                 (string-match "/oklinux-.*/" buffer-file-name) 
		 (string-match "/kernel-2.6" buffer-file-name)))
      (progn 
        (c-set-style "linux-tabs-only")
	(setq indent-tabs-mode t))
    (c-set-style "okl")
    (setq indent-tabs-mode nil)
    ;; do not align backslash in macro
    (setq c-auto-align-backslashes nil)
    )

    
  ;; we like auto-newline and hungry-delete
  ;;   (if (fboundp 'c-toggle-auto-newline)
  ;;       (c-toggle-auto-newline 1)
  ;;     (c-toggle-auto-state 1))

  ;; other customizations
  (setq
   tab-width 8           ; Tab characters are the normal 8 characters
   c-recognize-knr-p nil ; Don't look for old K&R functions.
   )
  ;; key bindings for all supported languages.  We can put these in
  ;; c-mode-base-map because c-mode-map, c++-mode-map, objc-mode-map,
  ;; java-mode-map, idl-mode-map, and pike-mode-map inherit from it.
  (define-key c-mode-base-map "\C-m" 'c-context-line-break))

(defun okl-show-trailing-whitespace ()
  (setq-default indent-tabs-mode nil)
  (setq show-trailing-whitespace t))

(add-hook 'c-mode-common-hook 'okl-c-mode-common-hook)
(add-hook 'ac-mode-common-hook 'okl-c-mode-common-hook)
(add-hook 'python-mode-hook 'okl-show-trailing-whitespace)
(add-hook 'asm-mode-hook 'okl-show-trailing-whitespace)
(add-hook 'rst-mode-hook 'okl-show-trailing-whitespace)
(add-hook 'emacs-lisp-mode-hook 'okl-show-trailing-whitespace)

(defun okl-reformat-buffer()
  (interactive)
  (universal-argument)
  (shell-command-on-region
   (point-min)
   (point-max)
   "okl_indent"
   t
   t
   )
  )

(define-key c-mode-base-map [f7] 'okl-reformat-buffer)


(add-to-list 'auto-mode-alist '("SCon.*" . python-mode))
(add-to-list 'auto-mode-alist '("\\.spp\\'" . asm-mode))
(add-to-list 'auto-mode-alist '("\\.cc.mi\\'" . c-mode))

(defun okl-asm-mode-hook()
 (setq-default
  defuault-tab-width 8   ; Tab characters are the normal 8 characters
  indent-tabs-mode nil   ; Don't use TABS in the files.
  tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120))
 )

(add-hook 'asm-mode-hook 'okl-asm-mode-hook)

;;   (setq
;;    tab-width 8           ; Tab characters are the normal 8 characters
;;    indent-tabs-mode nil  ; Don't use TABS in the files.
;;    )


(provide 'okl-style)

