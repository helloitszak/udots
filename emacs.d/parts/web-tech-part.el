(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
  (setq-default web-mode-code-indent-offset 2)
  (setq-default web-mode-markup-indent-offset 2)
  (setq-default web-mode-css-indent-offset 2))

(use-package json-mode
  :ensure t)

(use-package js2-mode
  :ensure t)

(provide 'web-tech-part)
