;;load paths
(add-to-list 'load-path "~/.emacs.d/vendor")
(add-to-list 'load-path "~/.emacs.d/vendor/pymacs-0.24-beta2")
(add-to-list 'load-path "~/.emacs.d/vendor/auto-complete-1.2")
(add-to-list 'load-path "~/.emacs.d/vendor/cc-mode")

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

;;(require 'pymacs)
;;(pymacs-load "ropemacs" "rope-")
;;(setq ropemacs-enable-autoimport t)

(autoload 'python-mode "my-py-setup" "" t)

(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/vendor/auto-complete-1.2/dict")
(ac-config-default)

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

(require 'linum)
(global-linum-mode 1)

;;c style
(setq c-set-style "ellemtel")
(setq c-default-style "ellemtel")

;;cedet
;(load-file "~/cedet-1.0.1/common/cedet.el")
;(global-ede-mode 1)                      ; Enable the Project management system
;(semantic-load-enable-code-helpers)      ; Enable prototype help and smart completion 
;(global-srecode-minor-mode 1)            ; Enable template insertion menu

;; llvm-mode
(load-file "~/.emacs.d/vendor/llvm/llvm-mode.el")
(load-file "~/.emacs.d/vendor/llvm/emacs.el")
(load-file "~/.emacs.d/vendor/llvm/tablegen-mode.el")

(setq mac-option-modifier 'meta)

(require 'xcscope)
(auto-fill-mode 1)
