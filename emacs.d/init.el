
;; setup gui
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(set-frame-font "Droid-Sans-Mono-15" t)

;; setup packages
(require 'package)
(require 'cl)
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
;;    evil-leader
    markdown-mode
    go-mode
    company-go
    flycheck
    ))

(defun udots-packages-installed-p ()
  (loop for p in udots-packages
	when (not (package-installed-p p)) do (return nil)
	finally (return t)))

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

;; general hackery
(setq initial-major-mode 'text-mode)
(setq-default show-trailing-whitespace nil)
(show-paren-mode t)
(setq-default indent-tabs-mode nil)
(setq default-tab-width 2)
(blink-cursor-mode 0)
(fset 'yes-or-no-p 'y-or-n-p)
(electric-indent-mode)
(setq line-number-mode t)
(setq column-number-mode t)

;; emacs lisp
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq mode-name "Elisp")
            (rainbow-delimiters-mode t)))

;; company
(setq company-idle-delay 0.5)
(setq company-tooltip-limit 10)
(setq company-minimum-prefix-length 2)
(setq company-tooltip-flip-when-above t)

(global-company-mode 1)
(require 'company-go)


;; smartparens
(require 'smartparens-config)

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

;; wind-move
(global-set-key (kbd "C-c C-h") 'windmove-left)
(global-set-key (kbd "C-c C-j") 'windmove-down)
(global-set-key (kbd "C-c C-k") 'windmove-up)
(global-set-key (kbd "C-c C-l") 'windmove-right)


;; evil

;; go
(add-hook 'go-mode-hook (lambda ()
                          (set (make-local-variable 'company-backends) '(company-go))
                          (local-set-key (kbd "C-c C-f") 'gofmt)
                          (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
                          (local-set-key (kbd "C-c C-d") 'godoc-at-point)))

(add-hook 'before-save-hook 'gofmt-before-save)
