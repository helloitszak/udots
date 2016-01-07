;;; This has packages dealing with the general editing of any kind of code

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0.5)
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-flip-when-above t)
  (global-company-mode))

(use-package company-quickhelp
  :ensure t)

(use-package smartparens
  :ensure t
  :config
  (require 'smartparens-config))

;; M-; to comment and uncomment lines
(use-package evil-nerd-commenter
  :ensure t
  :config
  (evilnc-default-hotkeys))

(use-package rainbow-delimiters
  :ensure t)

(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode))

(provide 'code-editing-part)
