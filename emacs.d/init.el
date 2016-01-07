;; setup gui
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; setup packages
(require 'package)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)


(package-refresh-contents)
;; setup use-package, which is used later to actually install packages
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(setq use-package-verbose t)
(require 'use-package)

;; backups
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms `((".*" ,(concat user-emacs-directory "auto-save-list") t)))

;; history
(setq savehist-file (concat user-emacs-directory "savehist"))
(savehist-mode 1)
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(kill-ring
	search-ring
	regexp-search-ring))

;; make sentences end with single space.
;; Makes M-e (forward-sentence) and M-a (backward-sentence) work like a sane human
(setq sentence-end-double-space nil)

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

;; packages and package-specific
;; (use-package zenburn-theme :ensure t)
;; load all of these cause I might be in the "mood" for a theme
(use-package solarized-theme :ensure t)
(use-package moe-theme :ensure t)
(use-package spacemacs-theme :ensure t)

(defadvice load-theme (before theme-dont-propagate activate)
  (mapcar #'disable-theme custom-enabled-themes))

;; (load-theme 'moe-light t)
;; (load-theme 'moe-dark t)
(load-theme 'spacemacs-dark t)
;; (load-theme 'solarized-dark t)

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
  (require 'smartparens-config)
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions :rem)
  (add-hook 'clojure-mode-hook (lambda () (smartparens-strict-mode t))))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode 1)
  (setq which-key-idle-delay 0.5))

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize))
  (exec-path-from-shell-copy-env "GOPATH"))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook (lambda () (rainbow-delimiters-mode t)))
  (add-hook 'clojure-mode-hook (lambda () (rainbow-delimiters-mode t))))

(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode))

(use-package magit
  :ensure t
  :bind
  (("C-c m" . magit-status)))

;; M-; to comment and uncomment lines
(use-package evil-nerd-commenter
  :ensure t
  :config
  (evilnc-default-hotkeys))

;; Org mode and friends
(use-package org
  :ensure t)

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode t))))

(use-package clojure-mode
  :ensure t)

(use-package cider
  :ensure t
  :config
  (add-hook 'cider-mode-hook (lambda () (eldoc-mode t))))

(use-package web-mode
  :ensure t)

(provide 'init)
