;;; package --- my emacs config

;;; Commentary:

;; TODO: Configure spellchecing for English and Russian with respect to programming modes
;; TODO: Configure autoformating on save?
;; TODO: Does it possible to display flycheck erros in ivy buffer?
;; TODO: It will be good if color-identifiers-mode will highlight all names (especially attributes)

;; examples: https://pages.sachachua.com/.emacs.d/Sacha.html#org1bcc938

;;; Code:

;; garbage collection threshold
(setq gc-cons-threshold (* 128 1024 1024))

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq package-archives
      '(("gnu-elpa"     . "https://elpa.gnu.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa"        . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("melpa-stable" . 10)
        ("gnu-elpa"     . 5)
        ("melpa"        . 0)))

(require 'package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; use-package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; global config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq vc-follow-symlinks t
      visible-bell t)

;; (defvar python-binaries "~/.local/opt/emacs-venv/bin/")
;; (defvar python-interpreter (concat python-binaries "python"))
;; (defvar python-pylint (concat python-binaries "pylint"))
;; (defvar python-flake8 (concat python-binaries "flake8"))

(defvar python-interpreter "python3.10")
(defvar python-pylint "pylint")
(defvar python-flake8 "flake8")

(prefer-coding-system 'utf-8)
(set-charset-priority 'unicode)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; macos configs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; http://gnu.ist.utl.pt/software/emacs/manual/html_node/Mac-Input.html
(setq mac-command-modifier 'meta
      mac-option-modifier nil)

;; by default, emacs initialize some strange input source on macos
;; so, we redefine it to correct
;; also, automatic input-method setup will be helpfull on all platforms
(set-input-method "russian-computer")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; theme configs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(global-eldoc-mode -1)

;; hide text & icon in title bar
(setq frame-title-format nil
      ns-use-proxy-icon nil)

(fringe-mode 10)

(setq inhibit-startup-message t
      indent-tabs-mode nil

      ;; fix slowdown on unicode text
      inhibit-compacting-font-caches t)

(column-number-mode 1)
(global-visual-line-mode 1)

;; disable suspend frame commands
(global-set-key "\C-z" nil)
(global-set-key (kbd "C-x C-z") nil)
(put 'suspend-frame 'disabled t)

(use-package doom-themes
  :defines (doom-themes-enable-bold
            doom-themes-enable-italic)
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
	doom-modeline-env-python-executable python-interpreter)

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

;; TODO: dimmer works with bugs
;;       it periodically permanently dim windows.
(use-package dimmer
  :init
  (setq dimmer-adjustment-mode :foreground
	dimmer-fraction 0.3)
  :config
  (dimmer-configure-which-key)
  (dimmer-mode t))

(use-package color-identifiers-mode
  :defines (color-identifiers-coloring-method)
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

;; translate input sequences to English,
;; so we can use Emacs bindings while a non-default system layout is active.
(use-package reverse-im
  :ensure t
  :custom
  (reverse-im-input-methods '("russian-computer"))
  :config
  (reverse-im-mode t))

;; better navigation on windows layout
(global-set-key (kbd "C-c b") 'windmove-left)
(global-set-key (kbd "C-c f") 'windmove-right)
(global-set-key (kbd "C-c p") 'windmove-up)
(global-set-key (kbd "C-c n") 'windmove-down)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-subword-mode 1)
(delete-selection-mode 1)
(winner-mode 1)
(global-auto-revert-mode t)

(setq-default indent-tabs-mode nil)

(desktop-save-mode)

(global-set-key (kbd "C-x k") 'kill-this-buffer)


;; https://lucidmanager.org/productivity/ricing-org-mode/
;; https://orgmode.org/worg/org-configs/org-customization-guide.html
;; examples: http://eschulte.github.io/org-scraps/
;;           http://doc.norang.ca/org-mode.html
(use-package org
  :mode (("\\.org$" . org-mode))
  :defines (org-id-link-to-org-use-id
	    org-export-coding-system
            org-export-with-sub-superscripts)
  :init
  (setq org-directory "~/repos/mine/my-org-base"
	org-startup-folded 'content
	org-tags-column 80
	org-return-follows-link nil
	org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id
	org-export-coding-system 'utf-8
	org-enforce-todo-dependencies t

	org-hide-leading-stars nil

	org-pretty-entities t
	org-hide-emphasis-markers t

	org-startup-with-inline-images t
	org-image-actual-width '(300)

        org-export-with-sub-superscripts nil
        org-use-sub-superscripts '{}

	org-modules '(org-id)
	org-todo-keywords '((sequence "TODO" "WORKING" "DONE")
			    (sequence "|" "CANCELED")))

  :bind (("C-c l" . 'org-store-link)
	 ("C-c a" . 'org-agenda)
	 ("C-c c" . 'org-capture)))


(use-package org-superstar
  :defines (org-superstar-special-todo-items)
  :config
  (setq org-superstar-special-todo-items t
	org-superstar-leading-bullet ?\s
	org-superstar-headline-bullets-list '(?‣ ?○ ?• ?- ?= ?⁖ ?⁘ ?⁙))
  (add-hook 'org-mode-hook (lambda ()
                             (org-superstar-mode 1))))

(use-package devdocs)

(use-package graphviz-dot-mode)

(use-package yaml-mode
  ;; :mode ("\\.yaml\\'" "\\.yml\\'")
  :custom-face
  (font-lock-variable-name-face ((t (:foreground "violet")))))


(use-package markdown-mode
  :bind (("C-c o" . 'markdown-follow-link-at-point)))


(use-package jinja2-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.j2\\'" . jinja2-mode)))

(use-package which-key
  :config
  (which-key-mode))

(use-package julia-mode
   :init
   (add-to-list 'auto-mode-alist '("\\.jl\\'" . typescript-mode)))
(use-package json-mode
   :init
   (add-to-list 'auto-mode-alist '("\\.json\\'" . typescript-mode)))
(use-package typescript-mode
   :init
   (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
   (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode)))

(use-package flycheck
  :init
  (setq flycheck-highlighting-mode 'sexps
	flycheck-check-syntax-automatically '(mode-enabled save)
	flycheck-pylint-use-symbolic-id nil
	flycheck-python-pylint-executable python-pylint
	flycheck-python-flake8-executable python-flake8)
  :config
  (global-flycheck-mode))

(use-package smartparens
  :init
  (require 'smartparens-config))


(use-package terraform-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.tf\\'" . terraform-mode)))

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

(use-package dockerfile-mode)

(use-package docker-compose-mode)

(use-package counsel)

(defun tiendil-file-jump-from-find ()
  "Switch to `counsel-file-jump' from `counsel-find-file'."
  (interactive)
  (ivy-quit-and-run
    (counsel-file-jump ivy-text ivy--directory)))

(defun tiendil-prepare-file-search-regexp ()
  "Prepare regexp for `counsel-find-file-ignore-regexp'."
  (concat (regexp-opt completion-ignored-extensions) "$"))

;; about swiper search optimization https://oremacs.com/2019/04/07/swiper-isearch/
;; TODO: choose counsel-grep instead of swiper-isearch if delays begin to annoy you
;;       but remember, that counsel-grep does not work with fileless buffers
;;       you should write a custom function to use swiper-isearch in such buffers
;; TODO: swiper-isearch breaks when first supplied character is '$'
;; TODO: command to open all filtered files
;;       see https://emacs.stackexchange.com/questions/38803/prelude-ivy-how-to-select-and-open-multiple-files
;;       solution works for counsel-find-file, but does not work for counsel-jump-file
(use-package swiper
  :defines (counsel-find-file-ignore-regexp)
  :requires ivy
  :bind (("C-s" . 'swiper-isearch)
	 ("M-x" . 'counsel-M-x)
	 ("C-x C-f" . 'counsel-find-file)
  	 ("C-x C-j" . 'counsel-file-jump)
	 :map counsel-find-file-map
	 ("C-m" . ivy-partial-or-done)
	 ("`" . 'tiendil-file-jump-from-find))
  :init
  (add-to-list 'completion-ignored-extensions "#")
  (add-to-list 'completion-ignored-extensions ".cache")
  (setq ivy-height 30
	ivy-use-virtual-buffers t
	ivy-count-format "(%d/%d) "
	ivy-extra-directories nil
	counsel-find-file-ignore-regexp (tiendil-prepare-file-search-regexp)
	ivy-re-builders-alist '((t . ivy--regex-plus)))

  :config
  (ivy-mode 1))


;; currentrly, should be installed manually from gnu-elpa, to upgrade default package
;; TODO: find a way to install automatically,
(use-package xref
  :ensure t
  :pin gnu-elpa
  :init
  (setq xref-backend-functions (remq 'etags--xref-backend xref-backend-functions)))

(use-package ivy-xref
  :ensure t
  :init
  (setq xref-show-definitions-function #'ivy-xref-show-defs))

(use-package dumb-jump
  ;; :hook (xref-backend-functions . dumb-jump-xref-activate)
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read))

(use-package avy
  :bind (("C-g" . 'avy-goto-line)
	 ("C-'" . 'avy-goto-char-2)))

(use-package undo-tree
  :bind (("C-c u" . undo-tree-visualize))
  :init
  (setq undo-tree-visualizer-diff 1
	undo-tree-visualizer-timestamps 1)
  :config
  (global-undo-tree-mode))

(use-package volatile-highlights
  :config
  (volatile-highlights-mode t))

(use-package company
  :bind (;;("M-." . company-complete)
	 :map company-active-map
	 ("C-n" . 'company-select-next)
	 ("C-e" . 'company-select-next)
	 ("C-p" . 'company-select-previous)
	 ("C-a" . 'company-select-previous))
  :init
  (setq company-show-numbers 'left
	company-idle-delay nil)
  :config
  (global-company-mode))

(use-package prescient
  :init
  (setq prescient-history-length 5))

(use-package ivy-prescient
  :config
  (ivy-prescient-mode))

(use-package company-prescient
  :config
  (company-prescient-mode))

(use-package command-log-mode
  :bind (("C-c C-l" . clm/open-command-log-buffer)))

(use-package magit)

(use-package magit-gitflow
  :hook (magit-mode . turn-on-magit-gitflow))

(use-package cheatsheet
  :bind (("<f1>" . 'cheatsheet-show))
  :config
  (cheatsheet-add-group '"Autocomplete"
			'(:key "M-." :description "display autocomplete list")
			'(:key "M-<0…9>" :description "choose suggestion"))
  (cheatsheet-add-group '"Open files"
			'(:key "C-x C-f" :description "open file with ivy completion")
			'(:key "` (from ivy)" :description "jump to file with recursive completion from current directory while opening file")
			'(:key "C-c a (from ivy)" :description "open all filtered files")

			'(:key "C-x C-j" :description "open file with recursive completion from current directory"))
  (cheatsheet-add-group '"Go to"
			'(:key "C-g" :description "go to line")
			'(:key "C-'" :description "go to position"))
  (cheatsheet-add-group '"Jump to"
			;; '(:key "C-c o" :description "jump to definition in other frame")
			'(:key "C-c o" :description "jump to link in markdown file"))
			;; '(:key "C-c j" :description "jump to definition in current frame"))
  (cheatsheet-add-group '"Undo tree"
			'(:key "C-c u" :description "open undo tree3"))
  (cheatsheet-add-group '"Commands Log"
			'(:key "C-x C-l" :description "open commands log buffer"))
  (cheatsheet-add-group '"Windows management"
			'(:key "C-x left" :description "undor last window hierarhy changes")
			'(:key "C-x right" :description "apply next window hierarhy changes")
			'(:key "C-x +" :description "balance windows sizes")))

(provide '.emacs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fixes/hacks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; disable emacs customize functions
;; copied from https://github.com/hlissner/doom-emacs/blob/develop/core/core-ui.el#L685-L696
(dolist (sym '(customize-option customize-browse customize-group customize-face
               customize-rogue customize-saved customize-apropos
               customize-changed customize-unsaved customize-variable
               customize-set-value customize-customized customize-set-variable
               customize-apropos-faces customize-save-variable
               customize-apropos-groups customize-apropos-options
               customize-changed-options customize-save-customized))
  (put sym 'disabled "do not support `customize'"))


(message "*** Emacs loaded in %s with %d garbage collections."
     (format "%.2f seconds"
             (float-time
              (time-subtract after-init-time before-init-time))) gcs-done)

;;; .emacs ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(terraform-mode xref ivy-xref magit-gitflow yascroll which-key web-mode volatile-highlights use-package undo-tree typescript-mode smartparens reverse-im org-superstar minions markdown-mode magit julia-mode json-mode jinja2-mode ivy-prescient highlight-parentheses graphviz-dot-mode flycheck dumb-jump doom-themes doom-modeline dockerfile-mode docker-compose-mode dimmer devdocs counsel company-prescient command-log-mode color-identifiers-mode cheatsheet avy)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-variable-name-face ((t (:foreground "violet")))))
