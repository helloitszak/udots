;; Allows setting of "customize" variables
(defmacro csetq (variable value)
  `(funcall (or (get ',variable 'custom-set) 'set-default) ',variable ,value))

;; Cleanup GUI Crud
(csetq tool-bar-mode nil)
(csetq scroll-bar-mode nil)

(csetq inhibit-startup-screen t)
(csetq initial-scratch-message "")

;; Navigation within a buffer
(csetq next-screen-context-lines 5)
(csetq recenter-positions '(top middle bottom))

;; Finding files
(csetq vc-follow-symlinks t)
(csetq find-file-suppress-same-file-warnings t)
(csetq find-file-name-completion-ignore-case t)
(csetq read-buffer-completion-ignore-case t)
(prefer-coding-system 'utf-8)

;; Editor Quality of life
(csetq indent-tabs-mode nil)
(csetq truncate-lines t)
(setq ring-bell-function 'ignore)
(defalias 'yes-or-no-p 'y-or-n-p)
(csetq confirm-kill-emacs 'y-or-n-p)
(csetq line-number-mode t)
(csetq column-number-mode t)
(blink-cursor-mode 0)
(csetq cursor-type 'bar)
(setq sentence-end-double-space nil)
(csetq indicate-empty-lines t)

;; Emacs Lisp
(csetq load-prefer-newer t)

;; Backups
(setq backup-directory-alist `(("." . ,(expand-file-name "backups" user-emacs-directory))))
(setq backup-by-copying t)
(setq delete-old-versions t)
(setq version-control t)
(setq create-lockfiles nil)

;; Make https
(csetq tls-checktrust t)
(csetq gnutls-verify-error t)

;; package.el
(setq package-archives
      '(("org" . "https://orgmode.org/elpa/")
        ("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; Make sure use-package is installed
(unless (package-installed-p 'use-package)
  (package-install 'use-package))


;; Custom (general) Keybinds
(global-unset-key (kbd "s-q"))
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "<s-return>") 'toggle-frame-fullscreen)

(require 'use-package)

;; Theme
(use-package solarized-theme
  :disabled
  :ensure t
  :config
  (load-theme 'solarized-light t))

(use-package moe-theme
  :disabled
  :ensure t
  :config
  (load-theme 'moe-light t))

(use-package gruvbox-theme
  :ensure t
  :config
  (load-theme 'gruvbox-light-medium t))


;; Handy Utilities
(defun udots/start-or-switch-to (function buffer-name)
  "Invoke FUNCTION if there is no buffer with BUFFER-NAME.
Otherwise switch to the buffer named BUFFER-NAME.  Don't clobber
the current buffer."
  (if (not (get-buffer buffer-name))
      (progn
        (split-window-sensibly (selected-window))
        (other-window 1)
        (funcall function))
    (switch-to-buffer-other-window buffer-name)))

;;(load-theme 'solarized-dark t)

;; PATH Mangling
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-env "GOPATH")))

;; Server
(use-package server
  :if window-system
  :init
  (add-hook 'after-init-hook 'server-start t))

;; Undo Tree
(use-package undo-tree
  :ensure t)

;; Multiple Cursors
(use-package multiple-cursors
              :ensure t)

;; Ibuffer
(use-package ibuffer
  :bind
  ("C-x C-b" . ibuffer)
  :config
  (setq ibuffer-saved-filter-groupsq
        '(("default"
           ("dired" (mode . dired-mode))
           ("Help" (or
                    (name . "\*Help\*")
                    (name . "\*Apropos\*")
                    (name . "\*Help\*")))
           ("emacs" (or
                     (name . "^\\*scratch\\*$")
                     (name . "^\\*Messages\\*$")))))))
;; Eshell
(use-package eshell
  :bind
  ("C-x m" . eshell))

(use-package magit
  :ensure t
  :bind
  ("C-x g" . magit-status))

;; Discovery
(use-package discover-my-major
  :ensure t
  :bind
  (:map help-map
        ("C-m" . discover-my-major)))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode 1)
  (setq which-key-idle-delay 0.5))


;; Ivy / Counsel / Swiper
(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (csetq ivy-display-style 'fancy)
  (csetq ivy-use-virtual-buffers t)
  (csetq counsel-find-file-ignore-regexp "\\`\\.")

  :bind
  (:map ivy-minibuffer-map
        ("C-c o" . ivy-occur)))

(use-package counsel
  :ensure t

  :bind
  ("M-x" . counsel-M-x)
  ("C-c k" . counsel-ag)
  ("C-x C-f" . counsel-find-file)
  ("C-h f" . counsel-describe-function)
  ("C-h v" . counsel-describe-variable))

(use-package swiper
  :ensure t
  :bind
  ("C-s" . swiper))

(use-package ivy-hydra
  :ensure t)

;; Company mode
(use-package company
  :ensure t
  :bind
  ("C-<tab>" . company-complete)
  :config
  (setq company-idle-delay 0.5)
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-flip-when-above t)
  (global-company-mode))

;; Projectile
;; http://batsov.com/projectile/
(use-package projectile
  :ensure t
  :init
  (projectile-mode)
  (setq projectile-completion-system 'ivy))

(use-package counsel-projectile
  :ensure t
  :config
  (counsel-projectile-on))

;; Emacs Lisp
(use-package lisp-mode
  :config
  (defun udots/visit-ielm ()
    "Switch to `ielm' buffer.
Starts `ielm' if it's not already running."
    (interactive)
    (udots/start-or-switch-to 'ielm "*ielm*"))
  :bind
  (:map emacs-lisp-mode-map
        ("C-c C-z" . udots/visit-ielm)))

(use-package macrostep
  :ensure t
  :bind
  (:map emacs-lisp-mode-map
        ("C-c e m" . macrostep-expand)))

;; General Lisps
(use-package lispy
  :ensure t)

(use-package aggressive-indent
  :ensure t)

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; Clojure
(use-package clojure-mode
  :ensure t)

(use-package cider
  :ensure t)

;; Elixir
(use-package elixir-mode
  :ensure t)

(use-package alchemist
  :ensure t)

;; Go
(use-package go-mode
  :ensure t)

(use-package company-go
  :ensure t
  :config
  (add-to-list 'company-backends 'company-go))

(use-package go-eldoc
  :ensure t
  :config
  (add-hook 'go-mode-hook 'go-eldoc-setup))

;; Clojure
(use-package clojure-mode
  :ensure t)

(use-package cider
  :ensure t)

;; Haskell
(use-package haskell-mode
  :ensure t)

(use-package intero
  :ensure t)
