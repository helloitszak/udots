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
(csetq ring-bell-function 'ignore)
(defalias 'yes-or-no-p 'y-or-n-p)
(csetq confirm-kill-emacs 'y-or-n-p)
(csetq line-number-mode t)
(csetq column-number-mode t)
(blink-cursor-mode 0)
(csetq cursor-type 'bar)
(csetq sentence-end-double-space nil)
(csetq indicate-empty-lines t)
(csetq save-interprogram-paste-before-kill t)

;; Emacs Lisp
(csetq load-prefer-newer t)

;; Backups
(setq backup-directory-alist `(("." . ,(expand-file-name "backups" user-emacs-directory))))
(setq backup-by-copying t)
(setq delete-old-versions t)
(setq version-control t)
(setq create-lockfiles nil)

;; Early load utilities
(defun udots/mac-p ()
  "Am I running on a mac"
  (memq window-system '(mac ns)))

;; Make https
;; This requires openssl to be installed through homebrew
(when (udots/mac-p)
  (require 'gnutls)
  (add-to-list 'gnutls-trustfiles "/usr/local/etc/openssl/cert.pem"))

(csetq tls-checktrust t)
(csetq gnutls-verify-error t)

;; package.el
(setq package-archives
      '(("org" . "https://orgmode.org/elpa/")
        ("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; Make sure use-package is installed
(unless (package-installed-p 'use-package)
  (package-install 'use-package))


;; Custom (general) Keybinds
(global-unset-key (kbd "s-q"))
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "<s-return>") 'toggle-frame-fullscreen)


;; There's no place like vim
(global-set-key (kbd "s-h") 'backward-char)
(global-set-key (kbd "s-j") 'next-line)
(global-set-key (kbd "s-k") 'previous-line)
(global-set-key (kbd "s-l") 'forward-char)


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

(defun lights-on ()
  (interactive)
  (load-theme 'gruvbox-light-medium t))

(defun lights-off ()
  (interactive)
  (load-theme 'gruvbox-dark-medium t))


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


(defun udots/update-bitbar-plugin (plugin)
  (if (udots/mac-p)
      (start-process "open-bitbar" nil "open" "-g" (format "bitbar://refreshPlugin?name=%s" plugin))
    (message "This is only available on macOS")))

;;(load-theme 'solarized-dark t)

;; PATH Mangling
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (udots/mac-p)
    (csetq exec-path-from-shell-arguments nil)
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
  (setq ibuffer-saved-filter-groups
        '(("default"
           ("dired" (mode . dired-mode))
           ("Help" (or
                    (name . "\*Help\*")
                    (name . "\*Apropos\*")
                    (name . "\*Help\*")))
           ("emacs" (or
                     (name . "^\\*scratch\\*$")
                     (name . "^\\*Messages\\*$")))))))

(use-package ibuffer-projectile
  :ensure t)


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
  (csetq ivy-initial-inputs-alist nil)
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

;; Expand region
(use-package expand-region
  :ensure t
  :bind
  ("C-=" . er/expand-region))

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
  (setq projectile-completion-system 'ivy)
  :bind 
  (:map
   projectile-mode-map 
   ("s-b" . projectile-switch-to-buffer)
   ("s-p" . projectile-command-map)))



(use-package counsel-projectile
  :ensure t
  :config
  (counsel-projectile-mode))

;; Org
(use-package org
  :ensure t
  :config
  (csetq org-src-window-setup 'current-window)
  :bind
  (("C-c c" . org-capture)
   :map
   org-mode-map
   ("C-c a" . org-agenda))

  

  :config
  ;; Setup
  (csetq org-agenda-files '("~/code/journal"
                            "~/code/journal/clients"))

  (csetq org-default-notes-file "~/code/journal/refile.org")

  ;; Refile
  (csetq org-refile-targets (quote ((nil :maxlevel . 9)
                                    (org-agenda-files :maxlevel . 9))))

  (csetq org-refile-allow-creating-parent-nodes (quote confirm))

  (csetq org-refile-use-outline-path 'file)
  (csetq org-outline-path-complete-in-steps nil)


  (defun udots/verify-refile-target ()
    "Exclude todo keywords with a done state from refile targets"
    (not (member (nth 2 (org-heading-components)) org-done-keywords)))
  
  (csetq org-refile-target-verify-function 'udots/verify-refile-target)
  
  ;; Clocking
  (defun udots/get-clock-for-menubar ()
    (if (org-clock-is-active)
        (substring-no-properties (org-clock-get-clock-string))
      "[Clock not running]"))

  (defun udots/update-bitbar-org-clock ()
    (udots/update-bitbar-plugin "org-clock.*?.sh"))
  
  (when (udots/mac-p)
    (add-hook 'org-clock-in-hook 'udots/update-bitbar-org-clock)
    (add-hook 'org-clock-out-hook 'udots/update-bitbar-org-clock)
    (add-hook 'org-clock-cancel-hook 'udots/update-bitbar-org-clock))
  
  (csetq org-clock-into-drawer t)
  (csetq org-clock-out-when-done t)
  (csetq org-clock-out-remove-zero-time-clocks t)
  (csetq org-clock-report-include-clocking-task t)

  ;; Properties
  (setq org-global-properties (quote (("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00"))))

  ;; Column
  (setq org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM")

  ;; Appearence
  (setq org-startup-indented t)

  ;; Logging
  (csetq org-log-done (quote time))
  (csetq org-log-into-drawer nil)
  (csetq org-log-state-notes-insert-after-drawers t)
  (csetq org-reverse-note-order nil)

  ;; Todo Configuration
  (csetq org-todo-keywords
         (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                 (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "CALL"))))

  (setq org-todo-keyword-faces
        (quote (("TODO" :foreground "red" :weight bold)
                ("NEXT" :foreground "blue" :weight bold)
                ("DONE" :foreground "forest green" :weight bold)
                ("WAITING" :foreground "orange" :weight bold)
                ("HOLD" :foreground "magenta" :weight bold)
                ("CANCELLED" :foreground "forest green" :weight bold)
                ("CALL" :foreground "forest green" :weight bold))))

  (csetq org-use-fast-todo-selection t)
  (csetq org-enforce-todo-dependencies t)

  ;; Tags
  ;; (csetq org-fast-tag-selection-single-key (quote expert))
  (csetq org-use-fast-tag-selection t)
  (csetq org-todo-state-tags-triggers
         (quote (("CANCELLED" ("CANCELLED" . t))
                 ("WAITING" ("WAITING" . t))
                 ("HOLD" ("WAITING") ("HOLD" . t))
                 (done ("WAITING") ("HOLD"))
                 ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
                 ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
                 ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

  ;; Capture Templates
  (csetq org-capture-templates
         (quote (("t" "todo" entry (file "~/code/journal/refile.org")
                  "* TODO %?\n%U\n" :clock-in t :clock-resume t)
                 ("n" "note" entry (file "~/code/journal/refile.org")
                  "* %? :NOTE:\n%U\n" :clock-in t :clock-resume t)
                 ("j" "Journal" entry (file+datetree "~/code/journal/journal.org")
                  "* %?\n%U\n" :clock-in t :clock-resume t)
                 )))

  ;; Agenda Helpers
  (defvar udots/hide-scheduled-and-waiting-next-tasks t)

  (defun udots/find-project-task ()
    "Move point to the parent (project) task if any"
    (save-restriction
      (widen)
      (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
        (while (org-up-heading-safe)
          (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
            (setq parent-task (point))))
        (goto-char parent-task)
        parent-task)))

  (defun udots/is-project-p ()
    "Any task with a todo keyword subtask"
    (save-restriction
      (widen)
      (let ((has-subtask)
            (subtree-end (save-excursion (org-end-of-subtree t)))
            (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
        (save-excursion
          (forward-line 1)
          (while (and (not has-subtask)
                      (< (point) subtree-end)
                      (re-search-forward "^\*+ " subtree-end t))
            (when (member (org-get-todo-state) org-todo-keywords-1)
              (setq has-subtask t))))
        (and is-a-task has-subtask))))

  (defun udots/is-project-subtree-p ()
    "Any task with a todo keyword that is in a project subtree.
Callers of this function already widen the buffer view."
    (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                                (point))))
      (save-excursion
        (udots/find-project-task)
        (if (equal (point) task)
            nil
          t))))

  (defun udots/is-task-p ()
    "Any task with a todo keyword and no subtask"
    (save-restriction
      (widen)
      (let ((has-subtask)
            (subtree-end (save-excursion (org-end-of-subtree t)))
            (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
        (save-excursion
          (forward-line 1)
          (while (and (not has-subtask)
                      (< (point) subtree-end)
                      (re-search-forward "^\*+ " subtree-end t))
            (when (member (org-get-todo-state) org-todo-keywords-1)
              (setq has-subtask t))))
        (and is-a-task (not has-subtask)))))

  (defun udots/is-subproject-p ()
    "Any task which is a subtask of another project"
    (let ((is-subproject)
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (while (and (not is-subproject) (org-up-heading-safe))
          (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
            (setq is-subproject t))))
      (and is-a-task is-subproject)))

  (defun udots/skip-non-projects ()
    "Skip trees that are not projects"
    (if (save-excursion (udots/skip-non-stuck-projects))
        (save-restriction
          (widen)
          (let ((subtree-end (save-excursion (org-end-of-subtree t))))
            (cond
             ((udots/is-project-p)
              nil)
             ((and (udots/is-project-subtree-p) (not (udots/is-task-p)))
              nil)
             (t
              subtree-end))))
      (save-excursion (org-end-of-subtree t))))

  (defun udots/skip-non-stuck-projects ()
    "Skip trees that are not stuck projects" 
    (save-restriction
      (widen)
      (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
        (if (udots/is-project-p)
            (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                   (has-next ))
              (save-excursion
                (forward-line 1)
                (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                  (unless (member "WAITING" (org-get-tags-at))
                    (setq has-next t))))
              (if has-next
                  next-headline
                nil)) ; a stuck project, has subtasks but no next task
          next-headline))))

  (defun udots/skip-non-project-tasks ()
    "Show project tasks.
Skip project and sub-project tasks, habits, and loose non-project tasks."
    (save-restriction
      (widen)
      (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
             (next-headline (save-excursion (or (outline-next-heading) (point-max)))))
        (cond
         ((udots/is-project-p)
          next-headline) 
         ((and (udots/is-project-subtree-p)
               (member (org-get-todo-state) (list "NEXT")))
          subtree-end)
         ((not (udots/is-project-subtree-p))
          subtree-end)
         (t
          nil)))))

  (defun udots/skip-projects-and-habits-and-single-tasks ()
    "Skip trees that are projects, tasks that are habits, single non-project tasks"
    (save-restriction
      (widen)
      (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
        (cond 
         ((member "WAITING" (org-get-tags-at))
          next-headline)
         ((udots/is-project-p)
          next-headline)
         ((and (udots/is-task-p) (not (udots/is-project-subtree-p)))
          next-headline)
         (t
          nil)))))

  (defun udots/skip-project-tasks ()
    "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
    (save-restriction
      (widen)
      (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
        (cond
         ((udots/is-project-p)
          subtree-end) 
         ((udots/is-project-subtree-p)
          subtree-end)
         (t
          nil)))))

  (defun udots/skip-non-tasks ()
    "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
    (save-restriction
      (widen)
      (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
        (cond
         ((udots/is-task-p)
          nil)
         (t
          next-headline)))))

  (defun udots/show-scheduled ()
    "Show the deadline"
    (or
     (org-entry-get (point) "SCHEDULED" nil)
     ""))

  (defun udots/org-agenda-prefix-string ()
    "Format"
    (let ((path (org-format-outline-path (org-get-outline-path))))
      (if (> (length path) 0)
          (concat " [" path "]"))))
  
  ;; Agenda
  (csetq org-agenda-tags-todo-honor-ignore-options t)
  (csetq org-agenda-dim-blocked-tasks nil)
  (csetq org-agenda-span 'day)
  (csetq org-agenda-custom-commands
         '((" " "Main Agenda" 
            ((agenda nil)
             (tags "REFILE"
                   ((org-agenda-overriding-header "Tasks to Refile")
                    (org-tags-match-list-sublevels nil)))
             
             (tags-todo "-HOLD-CANCELLED/!"
                        ((org-agenda-overriding-header "Projects")
                         (org-agenda-skip-function 'udots/skip-non-projects)
                         (org-tags-match-list-sublevels 'indented)
                         (org-agenda-sorting-strategy
                          '(category-keep))))

             (tags-todo "-CANCELLED/!"
                        ((org-agenda-overriding-header "Stuck Projects")
                         (org-agenda-skip-function 'udots/skip-non-stuck-projects)
                         (org-agenda-sorting-strategy
                          '(category-keep))))
             
             (tags-todo "-CANCELLED/!NEXT"
                        ((org-agenda-overriding-header "Project Next Tasks")
                         (org-agenda-skip-function 'udots/skip-projects-and-habits-and-single-tasks)
                         (org-tags-match-list-sublevels t)
                         (org-agenda-todo-ignore-scheduled 'future)
                         (org-agenda-todo-ignore-deadlines 'future) 
                         (org-agenda-sorting-strategy
                          '(todo-state-down effort-up category-keep))))

             (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                        ((org-agenda-overriding-header "Standalone Tasks")
                         (org-agenda-skip-function 'udots/skip-project-tasks)
                         (org-agenda-todo-ignore-scheduled 'future)
                         (org-agenda-todo-ignore-deadlines 'future) 
                         (org-agenda-sorting-strategy
                          '(category-keep))))

             (todo nil
                   ((org-agenda-overriding-header "Upcoming Tasks")
                    (org-agenda-prefix-format '((todo .  " %i %-11:c [%(udots/show-scheduled)] ")))
                    (org-agenda-skip-function '(org-agenda-skip-entry-if 'notscheduled))
                    (org-agenda-sorting-strategy
                     '(category-keep))
                    ))

             (tags-todo "-CANCELLED+WAITING|HOLD/!"
                        ((org-agenda-overriding-header "Waiting and Postponed Tasks")
                         (org-agenda-skip-function 'udots/skip-non-tasks)
                         (org-tags-match-list-sublevels nil)
                         (org-agenda-todo-ignore-scheduled 'future)
                         (org-agenda-todo-ignore-deadlines 'future))))))))




;; Markdown
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

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
  :init
  (csetq lispy-compat '(edebug cider))
  :ensure t)

(use-package aggressive-indent
  :ensure t)

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

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

;; Rust
(use-package rust-mode
  :mode "\\.rs\\'"
  :init
  (csetq rust-format-on-save t))

(use-package lsp-mode
  :ensure t
  :config
  (use-package lsp-flycheck
    :ensure f
    :after flycheck))

(use-package lsp-rust
  :ensure t
  :after lsp-mode)

;; Clojure
(use-package clojure-mode
  :ensure t)

(use-package cider
  :pin melpa-stable
  :ensure t)

(use-package clj-refactor
  :pin melpa-stable
  :ensure t)

;; Haskell
(use-package haskell-mode
  :ensure t)

(use-package intero
  :ensure t)

;; YAML
(use-package yaml-mode
  :ensure t)

;; Puppet
(use-package puppet-mode
  :ensure t)

;; Salt
(use-package salt-mode
  :ensure t)

;; Editorconfig
(use-package editorconfig
  :ensure t)

;; Lua
(use-package lua-mode
  :ensure t)

;; Ace-window
(use-package ace-window
  :bind
  ("M-o" . ace-window)
  :ensure t)


;; PHP
(use-package php-mode
  :ensure t)

;; Evil
(use-package evil
  :ensure t)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-support-shift-select t)
 '(package-selected-packages
   (quote
    (org-query php-mode evil lua-mode lua salt-mode lsp-rust ac-emmet lsp-mode yaml-mode which-key use-package undo-tree rainbow-delimiters puppet-mode markdown-mode magit macrostep lispy ivy-hydra intero ibuffer-projectile gruvbox-theme go-eldoc expand-region exec-path-from-shell editorconfig discover-my-major diminish counsel-projectile company-go clj-refactor alchemist aggressive-indent))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
