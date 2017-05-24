(use-package haskell-mode
  :ensure t
  :init
  (setq haskell-process-type 'cabal-repl))

;; (use-package flycheck-haskell
;;   :ensure t
;;   :config
;;   (add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))

(use-package intero
  :ensure t
  :config
  (add-hook 'haskell-mode-hook 'intero-mode))

;(use-package ghc-mod
;  :ensure t)

;(use-package
  

(provide 'haskell-part)
