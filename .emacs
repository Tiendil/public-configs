
;;; Code:

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(let ((default-directory "~/.emacs.d/elpa/"))
  (normal-top-level-add-subdirs-to-load-path))

(require 'package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; additional repos
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'package-archives '("elpa" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hideshow mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (require 'hideshow)

;; (defun toggle-selective-display (column)
;;       (interactive "P")
;;       (set-selective-display
;;        (or column
;;            (unless selective-display
;;              (1+ (current-column))))))

;; (global-set-key (kbd "M-+") 'toggle-selective-display)
;; (global-set-key (kbd "C-x C-r") 'repeat)

;; (add-hook 'c-mode-common-hook 'hs-minor-mode)
;; (add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
;; (add-hook 'java-mode-hook 'hs-minor-mode)
;; (add-hook 'lisp-mode-hook 'hs-minor-mode)
;; (add-hook 'perl-mode-hook 'hs-minor-mode)
;; (add-hook 'python-mode-hook 'hs-minor-mode)
;; (add-hook 'javascript-mode-hook 'hs-minor-mode)
;; (add-hook 'html-mode-hook 'hs-minor-mode)
;; (add-hook 'sh-mode-hook 'hs-minor-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; which-key mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'which-key)

(which-key-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; global config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-g") 'goto-line)

;; (setq transient-mark-mode t)

(setq-default indent-tabs-mode nil)

(global-visual-line-mode 1)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(global-eldoc-mode -1)
(setq inhibit-startup-message t)

(load-theme 'deeper-blue t)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; global modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(column-number-mode 1)
(global-subword-mode 1)
(delete-selection-mode 1)

(require 'julia-mode)

(require 'highlight-parentheses)
(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t)))
(global-highlight-parentheses-mode t)

(require 'highline)
(global-highline-mode t)
(set-face-background 'highline-face "black")

;; to update TAGS: find . -name "*.py" | etags --output TAGS -
;; (require 'etags-select)
;; (setq tags-revert-without-query t)
;; (visit-tags-table "~/repos/TAGS")
;; (global-set-key (kbd "M-.") 'etags-select-find-tag)

;; (require 'imenu)
;; (global-set-key (kbd "M-i") 'imenu)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;;
;; python modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'python-mode)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

;; (require 'auto-complete)
;; (global-auto-complete-mode 0) ;; disabled since unknown error occured in all buffers

(global-flycheck-mode)
;; (add-hook 'after-init-hook #'global-flycheck-mode)
;; (setq flycheck-display-errors-delay 0.25)

;; (use-package flycheck
;;   :ensure t
;;   :init (global-flycheck-mode))

;; (add-hook 'after-init-hook #'global-flycheck-mode)

(add-hook 'vue-mode-hook (lambda () (setq syntax-ppss-table nil)))

;; allow multiple major modes
;; (add-hook 'mmm-mode-hook
;;           (lambda ()
;;             (set-face-background 'mmm-default-submode-face nil)))


(setq visible-bell 1)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (which-key flycheck-julia julia-mode lsp-julia vue-mode php-mode bbcode-mode yaml-mode python-mode protobuf-mode markdown-mode jinja2-mode highline highlight-parentheses flycheck-color-mode-line etags-select auto-complete))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
