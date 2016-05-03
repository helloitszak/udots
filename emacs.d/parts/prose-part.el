(use-package markdown-mode
  :ensure t
  :config
  (setq markdown-command "cmark"))

(use-package pandoc-mode
  :ensure t)

(provide 'prose-part)
