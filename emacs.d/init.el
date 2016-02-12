;; setup gui
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(add-to-list 'default-frame-alist '(font . "Source Code Pro Regular-14"))

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

;; user custom packages
(add-to-list 'load-path (expand-file-name "parts" user-emacs-directory))

;; Emacs UI Enhancements and General Editor-Wide things.
(require 'emacs-enhancements-part)

;; This package is for config related to history and backup of files
(require 'backup-and-history-part)

;; Look and feel of Emacs
(require 'theme-part)

;; General packages for code editing
(require 'code-editing-part)

;; Org mode
(require 'org-part)

;; Prose (markdown, latex, etc.)
(require 'prose-part)

;; Code for emacs lisp
(require 'emacs-lisp-part)

;; Git (Magit, etc)
(require 'git-part)

;; Clojure, Cider, etc.
(require 'clojure-part)

;; Go
;; (exec-path-from-shell-copy-env "GOPATH")

;; C#
(require 'csharp-part)

;; Web Tech (html, css, js, etc.)
(require 'web-tech-part)

;; Sysadmin
(require 'sysadmin-part)

(provide 'init)
