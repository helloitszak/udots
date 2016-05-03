;; Org mode and friends
(use-package org
  :ensure t)

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode t))))

(provide 'org-part)
