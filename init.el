;;load paths
(add-to-list 'load-path "~/.emacs.d/vendor")
(add-to-list 'load-path "~/.emacs.d/vendor/pymacs-0.24-beta2")
(add-to-list 'load-path "~/.emacs.d/vendor/auto-complete-1.2")

;;start-up messages 
(setq inhibit-startup-message t)

;;want to see matching parens
(show-paren-mode t)

;;backup files
(setq backup-directory-alist '(("." . "~/.emacs.d/backup")))

;;
(setq ns-option-modifier (quote meta)) 

(setq-default indent-tabs-mode nil)    ; use only spaces and no tabs
(setq default-tab-width 4)

(require 'pymacs)
(pymacs-load "ropemacs" "rope-")
(setq ropemacs-enable-autoimport t)

(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/vendor/auto-complete-1.2/dict")
(ac-config-default)

(add-hook 'find-file-hook 'flymake-find-file-hook)
(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
               'flymake-create-temp-inplace))
       (local-file (file-relative-name
            temp-file
            (file-name-directory buffer-file-name))))
      (list "pycheckers"  (list local-file))))
   (add-to-list 'flymake-allowed-file-name-masks
             '("\\.py\\'" flymake-pyflakes-init)))
(load-library "flymake-cursor")
(global-set-key [f10] 'flymake-goto-prev-error)
(global-set-key [f11] 'flymake-goto-next-error)

(setq python-check-command "pyflakes")

;; yasnippet
(add-to-list 'load-path
              "~/.emacs.d/vendor/yasnippet")
(require 'yasnippet) ;; not yasnippet-bundle
(yas/global-mode 1)

;;txl setting
(require 'txl-mode)
(setq auto-mode-alist (cons (quote ("\\.\\([tT]xl\\|[gG]rm\\|[gG]rammar\\|[rR]ul\\(es\\)?\\|[mM]od\\(ule\\)?\\)$" . txl-mode)) auto-mode-alist))

;;org mode setting
(require 'org)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

(global-linum-mode 1)

;;c style
(setq c-set-style "ellemtel")
(setq c-default-style "ellemtel")

