;;; package --- my emacs config

;;; Commentary:

;; TODO: Configure spellchecing for English and Russian with respect to programming modes
;; TODO: Configure autoformating on save?
;; TODO: Does it possible to display flycheck erros in ivy buffer?

;;; Code:

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq package-archives
      '(("GNU ELPA"     . "https://elpa.gnu.org/packages/")
        ("MELPA Stable" . "https://stable.melpa.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("MELPA Stable" . 10)
        ("GNU ELPA"     . 5)
        ("MELPA"        . 0)))

(require 'package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; use-package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile (require 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; global config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq vc-follow-symlinks t
      visible-bell t)

(defvar python-shell-interpreter "python3")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; theme configs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(global-eldoc-mode -1)

(setq inhibit-startup-message t
      indent-tabs-mode nil)

(column-number-mode 1)
(global-visual-line-mode 1)

(use-package doom-themes
  :defines
  doom-themes-enable-bold
  doom-themes-enable-italic
  :init
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  :config
  ;; (load-theme 'doom-one t)
  ;; (load-theme 'doom-vibrant t)
  ;; (load-theme 'doom-city-lights t)
  ;; (load-theme 'doom-tomorrow-night t)
  ;; (load-theme 'doom-opera t)
  ;; (load-theme 'doom-peacock t)

  (load-theme 'doom-dracula t)
  (doom-themes-visual-bell-config))

(use-package minions
  :config
  (minions-mode 1))

(use-package doom-modeline
  :init
  (setq doom-modeline-buffer-file-name-style 'buffer-name
	doom-modeline-icon nil
	doom-modeline-unicode-fallback nil
	doom-modeline-minor-modes t
	doom-modeline-enable-word-count 1
	doom-modeline-continuous-word-count-modes '(markdown-mode)
	doom-modeline-buffer-encoding t
	doom-modeline-checker-simple-format t
	doom-modeline-number-limit 99
	doom-modeline-vcs-max-length 12

	doom-modeline-env-version t
	doom-modeline-env-python-executable python-shell-interpreter)

  :config
  (doom-modeline-mode 1))

(use-package hl-line
  :ensure nil
  :init
  (setq global-hl-line-sticky-flag t)
  :config
  (set-face-background hl-line-face "black")
  (global-hl-line-mode))

(use-package highlight-parentheses
  :config
  (global-highlight-parentheses-mode t))

(use-package uniquify
  :ensure nil
  :init
  (setq uniquify-buffer-name-style 'forward))

(use-package dimmer
  :init
  (setq dimmer-adjustment-mode :foreground
	dimmer-fraction 0.3)
  :config
  (dimmer-configure-which-key)
  (dimmer-mode t))

(use-package color-identifiers-mode
  :defines color-identifiers-coloring-method
  :init
  (setq color-identifiers-coloring-method :hash
	color-identifiers:min-color-saturation 0.0
	color-identifiers:max-color-saturation 1.0)
  :config
  (global-color-identifiers-mode))

(use-package yascroll
  :init
  (setq yascroll:delay-to-hide nil)
  :config
  (global-yascroll-bar-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-subword-mode 1)
(delete-selection-mode 1)

(use-package which-key
  :config
  (which-key-mode))

(use-package julia-mode)

(use-package flycheck
  :init
  (setq flycheck-highlighting-mode 'sexps
	flycheck-check-syntax-automatically '(mode-enabled save)
	flycheck-python-pylint-executable python-shell-interpreter)
  :config
  (global-flycheck-mode))

(use-package smartparens
  :init
  (require 'smartparens-config))

(use-package web-mode
  :init
  (setq web-mode-engines-alist '(("django" . "\\.html\\'"))
	web-mode-markup-indent-offset 2
	web-mode-css-indent-offset 2
	web-mode-code-indent-offset 2
	web-mode-style-padding 1
	web-mode-script-padding 1
	web-mode-block-padding 0)
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  :config
  ;; integration with smartparens-mode
  (setq web-mode-enable-auto-pairing nil)
  (defun sp-web-mode-is-code-context (id action context)
    (and (eq action 'insert)
	 (not (or (get-text-property (point) 'part-side)
		  (get-text-property (point) 'block-side)))))
  (sp-local-pair 'web-mode "<" nil :when '(sp-web-mode-is-code-context)))

(use-package ivy)

(use-package swiper
  :defines counsel-find-file-ignore-regexp
  :requires ivy
  :bind (("\C-s" . 'swiper)
	 ("C-c C-r" . 'ivy-resume)
	 ("M-x" . 'counsel-M-x)
	 ("C-x C-f" . 'counsel-find-file)
	 ("C-x C-j" . 'counsel-file-jump))
  :init
  (add-to-list 'completion-ignored-extensions "#")
  (add-to-list 'completion-ignored-extensions ".cache")
  (setq ivy-height 30
	ivy-use-virtual-buffers t
	ivy-count-format "(%d/%d) "
	counsel-find-file-ignore-regexp (regexp-opt completion-ignored-extensions)
	ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
  :config
  (ivy-mode 1))

(use-package prescient
  :init
  (setq prescient-history-length 5))

(use-package ivy-prescient)

(use-package dumb-jump
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external))
  :init
  (setq dumb-jump-selector 'ivy))

(use-package workgroups2
  :config
  (workgroups-mode 1))

(use-package avy
  :bind (("C-g" . 'avy-goto-line)
	 ("C-'" . 'avy-goto-char-2)))

(use-package undo-tree
  :init
  (setq undo-tree-visualizer-diff 1
	undo-tree-visualizer-timestamps 1)
  :config
  (global-undo-tree-mode))

(use-package cheatsheet
  :bind (("<f1>" . 'cheatsheet-show))
  :config
  (cheatsheet-add-group '"Open files"
			'(:key "C-x C-f" :description "open file with ivy completion")
			'(:key "C-x C-j" :description "jump to file from current directory with counsel completion")
			'(:key "C-u C-x C-j" :description "jump to file from base directory counsel completion"))
  (cheatsheet-add-group '"Go to"
			'(:key "C-x C-g" :description "go to line")
			'(:key "C-x '" :description "go to position"))
  (cheatsheet-add-group '"Jump to deinition"
			'(:key "M-g o" :description "jump to definition in other frame")
			'(:key "M-g j" :description "jump to definition in current frame")
			'(:key "M-g i" :description "write name and jump to it in current frame")
			'(:key "M-g x" :description "try to jump to definition in other frame")))

(provide '.emacs)
;;; .emacs ends here
