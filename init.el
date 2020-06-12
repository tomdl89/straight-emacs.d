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
(setq pop-up-windows nil)
(menu-bar-mode -1)
(tool-bar-mode -1)
(set-scroll-bar-mode 'right)
(setq ring-bell-function 'ignore)
(setq vc-follow-symlinks nil)
(blink-cursor-mode -1)
(defalias 'yes-or-no-p 'y-or-n-p)

(use-package zerodark-theme
  :config
  (load-theme 'zerodark t)
  (set-face-attribute 'default nil :height 100 :family "Fira Code"))

(use-package whitespace
  :init (setq whitespace-line-column 100
	      whitespace-style '(face tabs empty trailing lines-tail))
  :hook (prog-mode . whitespace-mode))

(use-package doom-modeline
  :init
  (doom-modeline-mode 1)
  (doom-modeline-def-segment purpose-status
    (let ((pms (purpose--modeline-string)))
      (and (string-match "[#!]+" pms)
	   (match-string 0       pms))))
  (doom-modeline-def-modeline 'tomline
    '(bar workspace-name window-number modals matches
	  buffer-info " " purpose-status remote-host selection-info)
    '(objed-state persp-name irc mu4e github debug lsp minor-modes
		  input-method " " major-mode process vcs checker))
  (defun setup-custom-doom-modeline ()
    (doom-modeline-set-modeline 'tomline 'default))
  (setup-custom-doom-modeline)
  :hook (doom-modeline-mode . setup-custom-doom-modeline)
  :config
  (setq doom-modeline-height 1)
  (set-face-attribute 'mode-line nil :height 90)
  (set-face-attribute 'mode-line-inactive nil :height 90)
  (setq doom-modeline-buffer-file-name-style 'relative-to-project)
  (setq which-function-mode nil))

(use-package general)

(use-package evil
  :init
  (evil-mode 1)
  (setq evil-cross-lines t
	evil-search-module 'evil-search
	evil-ex-search-vim-style-regexp t
	evil-magic 'very-magic
	evil-want-Y-yank-to-eol t)
  :general (:states           '(normal motion visual)
            "m"               'evil-next-visual-line
            "u"               'evil-previous-visual-line
            "n"               'evil-backward-char
            "h"               'evil-forward-char
            "k"               'evil-ex-search-next
            "K"               'evil-ex-search-previous
            "l"               'evil-set-marker
            "j"               'undo-tree-undo
            "gm"              'evil-next-line
            "gu"              'evil-previous-line
            "gj"              'evil-downcase
            "<S-tab>"         'evil-jump-backward
            "<S-iso-lefttab>" 'evil-jump-backward
            "/"               'evil-ex-search-forward
            "?"               'evil-ex-search-backward
            "<return>"        'evil-ex-nohighlight))

(use-package evil-anzu
  :init (global-anzu-mode))

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
  :init (setq evil-owl-idle-delay 0)
  :hook (prog-mode . evil-owl-mode))

(use-package evil-surround
  :config (global-evil-surround-mode 1))

(use-package evil-traces
  :init
  (evil-traces-mode)
  (evil-traces-use-diff-faces))

(use-package evil-textobj-line)
(use-package evil-textobj-entire)

(use-package so-long
  :init
  (global-so-long-mode)
  ;; Also related to long lines making emacs slow...
  (setq bidi-paragraph-direction 'left-to-right
	bidi-inhibit-bpa t))

(use-package hideshow
  :hook (prog-mode . hs-minor-mode))

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
   "<f3>g" 'counsel-git
   "C-f"   'counsel-rg)
  (:keymaps 'minibuffer-local-map "C-r" 'counsel-minibuffer-history))

(use-package avy
  :config (setq avy-keys '(?n ?t ?i ?e ?o ?s ?h ?a ?g ?y ?l ?w ?r ?d)
		avy-background t)
  :general (:states '(motion normal) "SPC" 'avy-goto-char))

(use-package company
  :config
  (setq company-idle-delay 0.5
	comapny-show-numbers t
	company-tooltip-limit 10
	company-minimum-prefix-length 2
	company-dabbrev-downcase nil
	company-tooltip-align-annotations t
	company-tooltip-flip-when-above t)
  (global-company-mode 1)
  :general (:states 'insert "<f4>" 'company-dabbrev))

(use-package rg
  :init (rg-enable-menu)
  :general (:states '(emacs normal insert visual) "C-S-f" 'rg-menu))

(use-package cl-lib
  :init
  (defun my-keyboard-escape-quit (fun &rest args)
    (cl-letf (((symbol-function 'one-window-p) (lambda (&rest _) t)))
      (apply fun args)))
  (advice-add 'keyboard-escape-quit :around #'my-keyboard-escape-quit))

(use-package magit
  :init (define-prefix-command 'personal-magit-map)
  :general
  (:keymaps '(magit-mode-map
	      magit-file-section-map
	      magit-unstaged-section-map
	      magit-staged-section-map
	      magit-hunk-section-map)
	    "u"        'evil-previous-line
	    "C-u"      'magit-unstage
	    "m"        'evil-next-line
	    "<C-m>"    'magit-merge ;; This will bind RET in terminal mode
	    "<return>" 'magit-visit-thing
	    "SPC"      'avy-goto-asterisk
	    "<C-tab>"  'ace-window
	    "£"        'switch-buffer-without-purpose
	    "<f3><f3>" 'find-file-without-purpose
	    "<f3>g"    'counsel-git
	    "<f3>p"    'purpose-find-file-overload
	    "<f3>r"    'counsel-recentf)
  (:keymaps '(global normal visual)
	    "M-m"   'personal-magit-map
	    "M-m m" 'magit-status
	    "M-m l" 'magit-log
	    "M-m b" 'magit-blame)
  (:keymaps 'magit-stash-section-map
	    "k" 'magit-stash-drop)
  (:keymaps 'git-rebase-mode-map
            "m" 'evil-next-line
	    "u" 'evil-previous-line)
  :config
  ;; This must be done _after_ loading, hence :config
  (add-hook 'after-save-hook 'magit-after-save-refresh-status t)
  (setq magit-diff-refine-hunk t))

(use-package restart-emacs
  :general ("<C-f5>" 'restart-emacs))

(use-package window-purpose
  :general (:states    '(emacs normal insert motion) 
	    "£"        'switch-buffer-without-purpose
	    "C-£"      'purpose-switch-buffer-with-purpose
	    "C-£"      'purpose-switch-buffer-with-purpose
	    "<f3><f3>" 'find-file-without-purpose
	    "<f3>p"    'purpose-find-file-overload
	    "<f7>"     'purpose-load-window-layout
	    "<C-f7>"   'purpose-save-window-layout)
  :init (purpose-mode)
  :config
  (add-to-list 'purpose-user-mode-purposes '(js2-mode . js))
  (add-to-list 'purpose-user-mode-purposes '(clojure-mode . clj))
  (add-to-list 'purpose-user-mode-purposes '(clojurec-mode . clj))
  (add-to-list 'purpose-user-mode-purposes '(clojurescript-mode . clj))
  (add-to-list 'purpose-user-mode-purposes '(cider-repl-mode . crm))
  (add-to-list 'purpose-user-mode-purposes '(magit-diff-mode . crm))
  (purpose-compile-user-configuration))

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
  :hook prog-mode cider-repl-mode
  :config
  (setq ccm-recenter-at-end-of-file t))

(use-package projectile)
(use-package projectile-git-autofetch
  :init (projectile-git-autofetch-mode 1)
  :after (projectile))

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

;; Disable auto-revert messages
(setq auto-revert-verbose nil)

;; Use anonymous start buffer
(defun anon-note ()
  (interactive)
  (get-buffer-create "**"))
(setq initial-buffer-choice 'anon-note)

(use-package highlight-sexp
  :init
  (setq hl-sexp-background-color "#303643")
  (global-highlight-sexp-mode))

(use-package hi-lock
  :config (set-face-attribute 'hi-yellow nil :background "#4e5565" :foreground "#abb2bf"))
(use-package highlight-thing
  :config (setq highlight-thing-delay-seconds 0.3)
  :general (:states 'normal "C-8" 'highlight-thing-mode))

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
  (prog-mode . smartparens-mode)
  (cider-repl-mode . smartparens-mode)
  (minibuffer-setup . conditionally-enable-smartparens-mode)
  :config
  (require 'smartparens-config)
  (setq smartparens-strict-mode t)
  (show-smartparens-global-mode t)
  (setq sp-base-key-bindings 'paredit
	sp-autoskip-closing-pair 'always)
  (sp-use-paredit-bindings)
  :general (:states '(normal insert)
	    "C-."   'sp-forward-slurp-sexp
	    "C-,"   'sp-backward-slurp-sexp
	    "C->"   'sp-forward-barf-sexp
	    "C-<"   'sp-backward-barf-sexp))

(use-package evil-cleverparens
  :hook
  (prog-mode . evil-cleverparens-mode)
  (cider-repl-mode . evil-cleverparens-mode)
  :general (:states  '(normal visual)
            :keymaps '(global evil-cleverparens-mode-map)
            "{"      'evil-backward-paragraph
            "}"      'evil-forward-paragraph
            "M-l"    'linum-mode
            "X"      'fixup-whitespace
            "("      'evil-previous-open-paren
            ")"      'evil-next-close-paren))

(use-package flycheck
  :init (global-flycheck-mode)
  :config (setq flycheck-indication-mode nil))
(use-package flycheck-clj-kondo
  :init (global-flycheck-mode))

(use-package clojure-mode
  :config (require 'flycheck-clj-kondo)
  :general
  (:keymaps 'clojure-mode-map
   "C-j"    'cider-jack-in-clj&cljs
   "C-S-r"  'lsp-rename)
  (:keymaps 'clojure-mode-map
   :states  'normal
   "gd"     'cider-find-var))
(use-package clojure-mode-extra-font-locking)
(use-package cider
  :config
  (add-to-list 'exec-path "~/bin/")
  (setq cider-show-error-buffer nil)
  :general
  (:keymaps 'cider-mode-map
	    "C-n"    'cider-repl-set-ns)
  (:keymaps 'cider-repl-mode-map
   :states '(normal insert)
   "¶"   'cider-repl-switch-to-other
   "M-i" 'cider-inspect)
  (:keymaps 'cider-stacktrace-mode-map
   :states 'normal
   "q" 'cider-popup-buffer-quit-function)
  (:keymaps 'cider-inspector-operate-on-point
   :states 'normal
   "<return>"        'cider-inspector-operate-on-point
   "l"               'cider-inspector-pop
   "g"               'cider-inspector-refresh
   "q"               'cider-popup-buffer-quit-function
   "<S-iso-lefttab>" 'cider-inspector-previous-inspectable-object
   "SPC"             'cider-inspector-next-page
   "M-SPC"           'cider-inspector-prev-page))
(use-package clj-refactor)

(use-package adoc-mode
  :init (add-to-list 'auto-mode-alist (cons "\\.txt\\'" 'adoc-mode)))

(use-package css-mode
  :init (setq css-indent-offset 2
	      css-fontify-colors nil))

(use-package scss-mode
  :init (add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode)))

(use-package lsp-mode
  :config
  (setq lsp-enable-snippet nil
	lsp-enable-indentation nil
	lsp-file-watch-threshold nil)
  (dolist (m '(clojure-mode
	       clojurec-mode
	       clojurescript-mode
	       clojurex-mode))
    (add-to-list 'lsp-language-id-configuration `(,m . "clojure")))
  :hook
  (clojure-mode . lsp)
  (clojurec-mode . lsp)
  (clojurescript-mode . lsp))

(use-package csv-mode)

(use-package dockerfile-mode
  :init (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

(use-package auctex
  :config 
  (defvar tex-fold-key-mode-map
    (make-keymap) "tex-fold-mode keymap.")
  (define-minor-mode tex-fold-key-mode
    "Mode to allow keybindings for tex folding"
    nil ;; Init-value
    " tex-folding"
    tex-fold-key-mode-map)
  :hook (TeX-mode . tex-fold-key-mode)
  :general (:states  'normal
	    :keymaps 'tex-fold-key-mode-map
	    "zc"     'TeX-fold-dwim
	    "zo"     'TeX-fold-dwim))

(defun open-init ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

;; Non-package-specific rebindings
(general-def
  :keymaps '(override normal insert)
  "C-c ESC"    'ignore
  "<f5>"       'balance-windows
  "M-£"        'kill-this-buffer
  "C-M-£"      'kill-some-buffers
  "<f3>i"      'open-init
  "C-a"        'mark-whole-buffer
  "<C-up>"     'enlarge-window
  "<C-S-up>"   'enlarge-window
  "<C-down>"   'shrink-window
  "<C-S-down>" 'shrink-window
  "<C-left>"  'enlarge-window-horizontally
  "<C-right>" 'shrink-window-horizontally)
(general-def 'override "<escape>" 'keyboard-escape-quit)
(general-def 'ctl-x-map [escape] 'ignore)
(general-def "C-6" 'evil-switch-to-windows-last-buffer)
(defun evil-insert-line-below-and-above ()
  "Open a line below and above the current line"
  (interactive)
  (evil-insert-newline-below)
  (evil-previous-line)
  (evil-insert-newline-above)
  (evil-next-line))
(general-def 'normal "C-o" 'evil-insert-line-below-and-above)

(use-package dired
  :straight nil :ensure nil
  :general (:states    'normal
	    :keymaps   'dired-mode-map
	    "<return>" 'dired-find-file
	    "w"        'wdired-change-to-wdired-mode
	    "m"        'dired-next-line
	    "u"        'dired-previous-line
	    "n"        'evil-backward-char
	    "h"        'evil-forward-char
	    "l"        'dired-mark
	    "L"        'dired-unmark
	    "C-S-l"    'dired-do-load
	    "<SPC>"    'avy-goto-line)
  :init (setq dired-listing-switches "-alBh"))
(use-package all-the-icons-dired
  :after (dired)
  :hook (dired-mode . all-the-icons-dired-mode))

;; Custom functions
;; (defun evil-paste-after-from-zero (count)
;;   "Paste after from yank register, rather than unnamed register"
;;   (interactive "P<x>") (evil-paste-after count ?0))
;; (defun evil-paste-before-from-zero (count)
;;   "Paste before from yank register, rather than unnamed register"
;;   (interactive "P<x>") (evil-paste-before count ?0))
;; (general-def
;;   :states           '(normal visual)
;;   :keymaps          '(global evil-mc-key-map)
;;   "C-p"             'evil-paste-after-from-zero
;;   "C-S-p"           'evil-paste-before-from-zero)

(evil-define-motion evil-search-symbol-forward (count &optional symbol)
  "Search forward for SYMBOL under point."
  :jump t
  :type exclusive
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     evil-symbol-word-search))
  (dotimes (var (or count 1))
    (evil-search-word t nil t)))

(evil-define-motion evil-search-symbol-backward (count &optional symbol)
  "Search backward for SYMBOL under point."
  :jump t
  :type exclusive
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     evil-symbol-word-search))
  (dotimes (var (or count 1))
    (evil-search-word nil nil t)))

(general-def
  :states '(normal motion visual)
  "*"     'evil-search-symbol-forward
  "M-*"   'evil-search-word-forward
  "#"     'evil-search-symbol-backward
  "M-#"   'evil-search-word-backward)

(defun evil-execute-q-macro (count)
  "Execute the q macro, the only one I use"
  (interactive "P<x>") (evil-execute-macro count "@q"))
(general-def 'normal "Q" 'evil-execute-q-macro)

;; Differentiate C-m from RET
(when (display-graphic-p)
  (general-def input-decode-map [?\C-m] [C-m]))

(defun repl-local-keys ()
  (general-def
    :keymaps          'local
    :states           '(normal insert visual)
    "<C-up>"          'cider-repl-previous-input
    "<C-down>"        'cider-repl-next-input
    "<C-return>"      'cider-repl-newline-and-indent))

(add-hook 'cider-repl-mode-hook 'repl-local-keys)

(defun ivy-occur-local-keys ()
  (general-def
    :keymaps   'local
    :states    '(normal motion visual)
    "<return>" 'ivy-occur-press-and-switch))

(add-hook 'ivy-occur-grep-mode-hook 'ivy-occur-local-keys)

(general-unbind '(normal motion) "C-e")
(general-unbind :keymaps '(evil-mc-key-map evil-normal-state-map) "C-n")

(general-def
  :keymaps 'cider-mode-map
  "C-e"    'cider-eval-sexp-at-point
  "C-n"    'cider-repl-set-ns)

(defun collapse-comments ()
  (interactive)
  (evil-ex "global/(comment/normal zc"))

(setq truncate-partial-width-windows t)
(defun truncate-partial-width-windows-50 ()
  (interactive)
  (if (eq truncate-partial-width-windows t)
      (setq truncate-partial-width-windows 50)
    (setq truncate-partial-width-windows t)))
(general-def "<f6>" 'truncate-partial-width-windows-50)

(defun lein-clean ()
  (interactive)
  (shell-command "lein clean"))
(general-def "C-l" 'lein-clean)

(use-package kaocha-runner
  :after (clojure-mode)
  :general (:keymaps 'clojure-mode-map
	    "C-c k t" 'kaocha-runner-run-test-at-point
	    "C-c k n" 'kaocha-runner-run-tests
	    "C-c k a" 'kaocha-runner-run-all-tests
	    "C-c k w" 'kaocha-runner-show-warnings
	    "C-c k h" 'kaocha-runner-hide-windows))

(use-package graphql-mode)
