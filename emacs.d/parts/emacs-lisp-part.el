;;; settings pertaining to editing elisp

(add-hook 'emacs-lisp-mode-hook (lambda () (rainbow-delimiters-mode t)))
(sp-local-pair 'emacs-lisp-mode "'" nil :actions :rem)

(provide 'emacs-lisp-part)
