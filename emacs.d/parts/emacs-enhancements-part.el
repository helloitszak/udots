;;; This package is for Emacs UI enhancements

;; make sentences end with single space.
;; Makes M-e (forward-sentence) and M-a (backward-sentence) work like a sane human
(setq sentence-end-double-space nil)

;; Don't let emacs quit itself
(setq confirm-kill-emacs 'y-or-n-p)

;; Prevent fat fingering
(global-unset-key (kbd "s-q"))

;; allows y or n in place of yes or no
(fset 'yes-or-no-p 'y-or-n-p)

;; default to 120 char fill
(setq-default fill-column 120)

;; highlights matching parens
(show-paren-mode t)

;; stop the damn cursor blinking
(blink-cursor-mode 0)

;; lines and columns
(setq line-number-mode t)
(setq column-number-mode t)

;; indent on newline
(electric-indent-mode)

;; set default major mode to text-mode
(setq initial-major-mode 'text-mode)

;; ido mode, for now
(ido-mode t)
(setq ido-enable-flex-matching t)

;; Make tab insert spaces by default
(setq-default indent-tabs-mode nil)

;; smex = ido for M-x
(use-package smex
  :ensure t
  :init
  (smex-initialize)
  :bind
  (("M-x" . smex)
   ("M-X" . smex-major-mode-commands)))

;; x = delete
;; m = swap window
;; v = split vertically
;; b = split horizontally
;; n = select previous
;; i = maximize window
;; o = maximize current window
(use-package ace-window
  :ensure t
  :config
  (setq aw-dispatch-always t)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind
  (("M-p" . ace-window)))

;; which-key is a popup guide that gives you all the options
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode 1)
  (setq which-key-idle-delay 0.5))

(use-package visual-regexp
  :ensure t)

(use-package ag
  :ensure t)

(use-package editorconfig
  :ensure t)

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-env "GOPATH")))

(global-set-key (kbd "<s-return>") 'toggle-frame-fullscreen)

(provide 'emacs-enhancements-part)
