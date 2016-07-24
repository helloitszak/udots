(use-package go-mode
  :ensure t
  :config
  (add-hook 'go-mode-hook
	    (lambda ()
	      (add-hook 'before-save-hook 'gofmt-before-save nil t)))
  :bind (:map go-mode-map
	      ("C-c C-r" . go-remove-unused-imports)
	      ("C-c C-g" . go-goto-imports)
	      ("C-c C-k" . godoc)
	      ("C-c C-f" . gofmt)))

(use-package company-go
  :ensure t
  :config
  (add-hook 'go-mode-hook
	    (lambda ()
	      (set (make-local-variable 'company-backends) '(company-go)))))


(use-package go-eldoc
  :ensure t
  :config
  (add-hook 'go-mode-hook 'go-eldoc-setup))

(provide 'go-part)
