(use-package clojure-mode
  :ensure t
  :config
  (add-hook 'clojure-mode-hook (lambda ()
			       (smartparens-strict-mode t)
			       (rainbow-delimiters-mode t))))
(use-package cider
  :ensure t
  :config
  (add-hook 'cider-mode-hook (lambda () (eldoc-mode t))))

(provide 'clojure-part)
