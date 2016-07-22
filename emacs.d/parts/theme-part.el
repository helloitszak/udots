;; packages and package-specific
;; (use-package zenburn-theme :ensure t)
;; load all of these cause I might be in the "mood" for a theme
(use-package solarized-theme :ensure t)
(use-package moe-theme :ensure t)
(use-package spacemacs-theme :ensure t)
(use-package zenburn-theme :ensure t)

(defadvice load-theme (before theme-dont-propagate activate)
  (mapcar #'disable-theme custom-enabled-themes))

;; (defvar udots/themelist (list 'moe-light))

;; (defun quicktheme (theme)
;;   (interactive (list (intern (completing-read "Theme: "
;; 					      (mapcar 'symbol-name
;; 						      udots/themelist))) nil nil))
;;   (load-theme theme t))


;; (load-theme 'moe-light t)
;; (load-theme 'spacemacs-dark t)
(load-theme 'solarized-dark t)

(provide 'theme-part)
