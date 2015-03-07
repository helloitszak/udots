;; setup gui
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(set-frame-font "Droid-Sans-Mono-15" t)
(add-to-list 'default-frame-alist
             '(font . "Droid-Sans-Mono-15"))

;; setup packages
(require 'package)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)


(defvar udots-packages
  '(
    exec-path-from-shell
    moe-theme
    company
    guide-key
    diminish
    smartparens
    rainbow-delimiters
    magit
    helm
    whitespace
    dired+
    evil
    evil-leader
    evil-nerd-commenter
    markdown-mode
    go-mode
    company-go
    flycheck
    editorconfig
    org
    git-gutter
    js2-mode
    projectile
    helm-projectile
    helm-ag
    web-mode
    ))

(defun udots-packages-installed-p ()
  "check if all packages are installed"
  (catch 'udots-package-installed
    (dolist (p udots-packages)
      (when (not (package-installed-p p))
        (throw 'udots-package-installed nil)))
    t))

(defun udots-list-foreign-packages ()
  "lists packages not part of udots"
  (interactive)
  (package-show-package-list
   (set-difference package-activated-list udots-packages)))

(defun udots-install-packages ()
  "installs udots packages"
  (interactive)
  (unless (udots-packages-installed-p)
    (message "%s" "Now refreshing package database...")
    (package-refresh-contents)
    (message "%s" "Done")
    (dolist (p udots-packages)
      (when (not (package-installed-p p))
	(package-install p)))))

(udots-install-packages)

;; PATH
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))
(exec-path-from-shell-copy-env "GOPATH")

;; theme
(require 'moe-theme)
(moe-light)

;; backups
(defvar udots-backup-directory (concat user-emacs-directory "backups"))
(if (not (file-exists-p udots-backup-directory))
    (make-directory udots-backup-directory t))

(setq backup-directory-alist `(("." . ,udots-backup-directory)))
(setq make-backup-files t
      backup-by-copying t
      version-control t
      delete-old-versions t
      kept-old-versions 6
      kept-new-versions 9
      auto-save-default t
      auto-save-timeout 20
      auto-save-interval 200)

;; general hackery
(setq initial-major-mode 'text-mode)
(setq-default show-trailing-whitespace t)
(show-paren-mode t)
(setq-default indent-tabs-mode nil)
(setq tab-width 2)
(blink-cursor-mode 0)
(fset 'yes-or-no-p 'y-or-n-p)
(electric-indent-mode)
(setq line-number-mode t)
(setq column-number-mode t)

;; make <escape> quit as much as possible
;; s/o to spacemacs for this tip
(define-key minibuffer-local-map (kbd "<escape>") 'keyboard-escape-quit)
(define-key evil-visual-state-map (kbd "<escape>") 'keyboard-quit)
(define-key minibuffer-local-ns-map (kbd "<escape>") 'keyboard-escape-quit)
(define-key minibuffer-local-completion-map (kbd "<escape>") 'keyboard-escape-quit)
(define-key minibuffer-local-must-match-map (kbd "<escape>") 'keyboard-escape-quit)
(define-key minibuffer-local-isearch-map (kbd "<escape>") 'keyboard-escape-quit)

;; emacs lisp
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq flycheck-disabled-checkers '(emacs-lisp-checkdoc))
            (setq mode-name "Elisp")
            (rainbow-delimiters-mode t)))

;; company
(setq company-idle-delay 0.5)
(setq company-tooltip-limit 10)
(setq company-minimum-prefix-length 2)
(setq company-tooltip-flip-when-above t)

(global-company-mode 1)
(require 'company-go)

;; git gutter
(global-git-gutter-mode t)

;; smartparens
(require 'smartparens-config)

;;dired+
(require 'dired+)

;; flycheck
(add-hook 'after-init-hook
          #'global-flycheck-mode)

;; guide key
(setq guide-key/guide-key-sequence t)
(guide-key-mode 1)

;; ido mode
(ido-mode t)
(setq ido-enable-flex-matching t)

;; helm
(require 'helm-config)
(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p t
      helm-move-to-line-cycle-in-source t
      helm-ff-search-library-in-sexp t
      helm-scroll-amount 8
      helm-ff-file-name-history t)

(helm-mode t)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

;; projectile
(require 'helm-projectile)
(projectile-global-mode)

;; evil
(evil-mode t)
(define-key evil-motion-state-map (kbd "C-u") 'evil-scroll-up)
(evil-ex-define-cmd "b[uffer]" 'helm-buffers-list)
(evil-ex-define-cmd "e[dit]" 'find-file)

(define-key evil-ex-map "b" 'helm-buffers-list)
(define-key evil-ex-map "e" 'find-file)

;; evil-leader keybindings
(setq evil-leader/in-all-states t
      evil-leader/leader "SPC"
      evil-leader/non-normal-prefix "s-")

(global-evil-leader-mode)



(evil-leader/set-key
  "u" 'universal-argument
  "!" 'shell-command
  "b" 'helm-buffers-list
  ":" 'eval-expression
  "x" 'execute-extended-command)

(evil-leader/set-key
  "a u" 'undo-tree-visualize)

(evil-leader/set-key
  "g n" 'git-gutter:next-hunk
  "g p" 'git-gutter:previous-hunk
  "g h s" 'git-gutter:stage-hunk
  "g h r" 'git-gutter:revert-hunk
  "g s" 'magit-status)

(evil-leader/set-key
  "p p" 'helm-projectile-switch-project
  "p d" 'projectile-dired
  "p f" 'helm-projectile-find-file
  "p b" 'helm-projectile-switch-to-buffer
  "p a" 'helm-projectile-ag)

(evil-leader/set-key-for-mode 'emacs-lisp-mode
  "c x" 'eval-defun)

(evil-leader/set-key-for-mode 'go-mode
  "c f" 'gofmt
  "c r" 'go-remove-unused-imports
  "c d" 'godoc-at-point
  "c a" 'go-import-add)

;; go
(add-hook 'go-mode-hook (lambda ()
                          (set (make-local-variable 'company-backends) '(company-go))
                          (local-set-key (kbd "C-c C-f") 'gofmt)
                          (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
                          (local-set-key (kbd "C-c C-d") 'godoc-at-point)))

(add-hook 'before-save-hook 'gofmt-before-save)

;; webmode
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
