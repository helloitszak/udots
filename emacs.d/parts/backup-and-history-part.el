;;; This package is for config related to history and backup of files

;; backups
(setq backup-directory-alist `(("." . ,(expand-file-name "backups" user-emacs-directory))))
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms `((".*" ,(expand-file-name "auto-save-list" user-emacs-directory) t)))

;; history
(setq savehist-file (expand-file-name "savehist" user-emacs-directory))
(savehist-mode 1)
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(kill-ring
	search-ring
	regexp-search-ring))

(provide 'backup-and-history-part)
