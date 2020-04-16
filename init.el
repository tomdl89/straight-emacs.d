(setq gc-cons-threshold-original gc-cons-threshold)
(setq gc-cons-threshold (* 1024 1024 100))
(setq file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

(run-with-idle-timer
 5 nil
 (lambda ()
   (setq gc-cons-threshold gc-cons-threshold-original)
   (setq file-name-handler-alist file-name-handler-alist-original)
   (makunbound 'gc-cons-threshold-original)
   (makunbound 'file-name-handler-alist-original)
   (message "gc-cons-threshold and file-name-handler-alist restored")))

;; Bootstrap straight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(add-hook 'window-setup-hook 'toggle-frame-maximized t)
(add-hook 'window-setup-hook 'toggle-frame-fullscreen t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)

(use-package zerodark-theme
  :config (load-theme 'zerodark t))

(use-package doom-modeline
  :config (doom-modeline-mode 1))

(use-package general)

(use-package evil
  :config
  (evil-mode 1)
  (setq evil-cross-lines t
	evil-search-module 'evil-search
	evil-ex-search-vim-style-regexp t
	evil-magic 'very-magic
	evil-want-Y-yank-to-eol t)
  :general
  (:states '(normal motion visual)
	   "m"               'evil-next-visual-line
	   "u"               'evil-previous-visual-line
	   "n"               'evil-backward-char
	   "h"               'evil-forward-char
	   "k"               'evil-search-next
	   "K"               'evil-search-previous
	   "l"               'evil-set-marker
	   "j"               'undo-tree-undo
	   "gm"              'evil-next-line
	   "gu"              'evil-previous-line
	   "gj"              'evil-downcase
	   "<S-tab>"         'evil-jump-backward
	   "<S-iso-lefttab>" 'evil-jump-backward))

(use-package evil-anzu)

(use-package evil-exchange
  :general (:states '(normal visual) "gx" 'evil-exchange))

(use-package evil-commentary
  :hook (prog-mode . evil-commentary-mode))

(use-package evil-fringe-mark
  :hook (prog-mode . evil-fringe-mark-mode)
  :config (setq-default right-fringe-width 16
			evil-fringe-mark-side 'right-fringe
			evil-fringe-mark-show-special t))

(use-package evil-quickscope
  :hook (prog-mode . evil-quickscope-mode))

(use-package evil-owl
  :hook (prog-mode . evil-owl-mode))

(use-package evil-traces
  :init
  (evil-traces-mode)
  (evil-traces-use-diff-faces))

(use-package which-key
  :config (which-key-mode))

(use-package smex)

(use-package ivy
  :init (ivy-mode 1)
  :general (:keymaps 'ivy-mode-map "<C-return>" 'ivy-immediate-done))

(use-package counsel
  :general
  ("M-x"   'counsel-M-x
   "C-h v" 'counsel-describe-variable
   "C-h f" 'counsel-describe-function)
  (:states '(emacs normal insert visual)
	   "<f3>r" 'counsel-recentf
	   "<f3>g" 'counsel-git)
  (:keymaps 'minibuffer-local-map "C-r" 'counsel-minibuffer-history))

(use-package avy
  :config (setq avy-keys '(?n ?t ?i ?e ?o ?s ?h ?a ?g ?y ?l ?w ?r ?d))
  :general (:states '(motion normal) "SPC" 'avy-goto-char))

(use-package company
  :config
  (setq company-idle-delay 0.5
	comapny-show-numbers t
	company-tooltip-limit 10
	company-minimum-prefix-length 2
	company-tooltip-align-annotations t
	company-tooltip-flip-when-above t)
  (global-company-mode 1))

(use-package magit)

(use-package restart-emacs)

(use-package window-purpose
  :general (:states    '(emacs normal insert) 
	    "£"        'switch-buffer-without-purpose
	    "C-£"      'purpose-switch-buffer-with-purpose
	    "C-£"      'purpose-switch-buffer-with-purpose
	    "<f3><f3>" 'find-file-without-purpose
	    "<f3>p"    'purpose-find-file-overload))

(use-package windmove
  :config (windmove-default-keybindings))

(use-package ace-window
  :general ("C-<tab>" 'ace-window)
  :config (setq aw-dispatch-always t
		aw-keys '(?n ?i ?h ?y ?l ?r ?t ?e ?s ?a ?g ?w ?d)
		aw-scope 'frame))

(setq split-height-threshold 200)

(use-package centered-cursor-mode
  :general (:states 'normal "zz" 'centered-cursor-mode)
  :hook prog-mode
  :config
  (setq ccm-recenter-at-end-of-file t))

(use-package super-save
  :init (super-save-mode +1)
  :config
  (setq auto-save-default nil))

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

(recentf-mode +1)
(setq recentf-max-menu-items 200
      recentf-max-saved-items 200)
(run-at-time 300 300 'recentf-save-list)

;; Set column after which text is highlighted
(setq whitespace-line-column 100)

;; Set gdefault for substitutions
(setq-default evil-ex-substitute-global t)

;; Use anonymous start buffer
(defun anon-note ()
  (interactive)
  (get-buffer-create "**"))
(setq initial-buffer-choice 'anon-note)

(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :config (setq highlight-indent-guides-auto-odd-face-perc 20
		highlight-indent-guides-auto-even-face-perc 20
		highlight-indent-guides-auto-character-face-perc 5
		highlight-indent-guides-character ?\u2502
		highlight-indent-guides-method 'character
		highlight-indent-guides-responsive 'top))

(use-package highlight-thing
  :config (setq highlight-thing-delay-seconds 0.3))

(use-package midnight)

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

(use-package diff-hl
  :init (global-diff-hl-mode))

(defun conditionally-enable-smartparens-mode ()
  "Enable `smartparens-mode' in the minibuffer, during `eval-expression'."
  (if (eq this-command 'eval-expression)
      (smartparens-mode 1)))

(use-package smartparens
  :hook
  (lisp-mode . smartparens-mode)
  (emacs-lisp-mode . smartparens-mode)
  (clojure-mode . smartparens-mode)
  (minibuffer-setup . conditionally-enable-smartparens-mode)
  :config
  (require 'smartparens-config)
  (setq smartparens-strict-mode t)
  (show-smartparens-global-mode t)
  :general (:states '(normal insert)
		    "C-." 'sp-forward-slurp-sexp
		    "C-," 'sp-backward-slurp-sexp
		    "C->" 'sp-forward-barf-sexp
		    "C-<" 'sp-backward-barf-sexp))

(use-package evil-cleverparens
  :hook
  (lisp-mode . evil-cleverparens-mode)
  (emacs-lisp-mode . evil-cleverparens-mode)
  (clojure-mode . evil-cleverparens-mode)
  :general
  (:states '(normal visual)
	   :keymaps '(global evil-cleverparens-mode-map)
	   "{"      'evil-backward-paragraph
	   "}"      'evil-forward-paragraph
	   "M-l"    'linum-mode
	   "X"      'fixup-whitespace
	   "("      'evil-previous-open-paren
	   ")"      'evil-next-close-paren))

(use-package clojure-mode)
(use-package cider)
(use-package clj-refactor)

(use-package adoc-mode
  :init (add-to-list 'auto-mode-alist (cons "\\.txt\\'" 'adoc-mode)))

(defun open-init ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

;; Non-package-specific rebindings
(general-def
  :keymaps '(override normal insert)
  "C-c ESC"  'ignore
  "<f5>"     'balance-windows
  "M-£"      'kill-this-buffer
  "C-M-£"    'kill-some-buffers
  "<f3>i"    'open-init)
(general-def 'override "<escape>" 'keyboard-escape-quit)
(general-def 'ctl-x-map [escape] 'ignore)

(defun evil-insert-line-below-and-above ()
  "Open a line below and above the current line"
  (interactive)
  (evil-insert-newline-below)
  (evil-previous-line)
  (evil-insert-newline-above)
  (evil-next-line))
(general-def 'normal "C-o" 'evil-insert-line-below-and-above)
