;;
;; Feature mode for cucumber
;;
(require 'feature-mode)
(add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))

;;
;; For Gnu-plot
;;
(pine-add-load-path "gnuplot")
(autoload 'gnuplot-mode "gnuplot" "gnuplot major mode" t)
(autoload 'gnuplot-make-buffer "gnuplot" "open a buffer in gnuplot mode" t)
(setq auto-mode-alist (append '(("\\.gp$" . gnuplot-mode)) auto-mode-alist))


;;
;; For Plantuml to draw UML diagrams
;;
;; active Org-babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '(;; other Babel languages
   (plantuml . t)))

(setq org-plantuml-jar-path
      (expand-file-name "~/bin/plantuml.jar"))

;; Do not Confirm before evaluation.
(setq org-confirm-babel-evaluate nil)

(provide 'emacs-misc)

