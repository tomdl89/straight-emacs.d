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
(setq straight-check-for-modifications '(check-on-save find-when-checking))
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

(savehist-mode 1)
(add-hook 'window-setup-hook 'toggle-frame-maximized t)
(add-hook 'window-setup-hook 'toggle-frame-fullscreen t)
(setq pop-up-windows nil)
(menu-bar-mode -1)
(tool-bar-mode -1)
(set-scroll-bar-mode 'right)
(setq ring-bell-function 'ignore)
(setq vc-follow-symlinks t)
(setq blink-cursor-delay 60
      blink-cursor-blinks 0)
(blink-cursor-mode t)
(setq make-backup-files nil)
(setq auto-revert-interval 1)
(setq-default indent-tabs-mode nil)
(setq-default calc-multiplication-has-precedence nil)
(setq use-short-answers t)
(setq inhibit-startup-screen t)
(setq eval-expression-print-length nil
      eval-expression-print-level nil)
(setq x-selection-timeout 500)

(setq-default abbrev-mode t)
(when (file-exists-p "~/.abbrev_defs")
  (read-abbrev-file "~/.abbrev_defs"))
(setq save-abbrevs t)

(use-package general)

(use-package emacs
  :config (setq calendar-week-start-day 1)
  :custom
  (warning-minimum-level :error)
  (xref-show-definitions-function #'xref-show-definitions-buffer-at-bottom)
  :general (:states '(normal visual)
            :keymaps 'override
            "M-l"    'display-line-numbers-mode))

(use-package zerodark-theme
  :config
  (load-theme 'zerodark t)
  (set-face-attribute 'default nil :height 100 :family "Fira Code"))

;; (use-package whitespace
;;   :init (setq whitespace-line-column 100
;; 	      whitespace-style '(face tabs empty trailing lines-tail))
;;   :hook (prog-mode . whitespace-mode))
(setq-default fill-column 100)
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)

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

(use-package undo-fu)

(use-package goto-chg
  :general ("M-;" 'goto-last-change
            "M-," 'goto-last-change-reverse))

(use-package evil
  :init
  (setq evil-cross-lines t
        evil-jumps-cross-buffers nil
        evil-ex-search-vim-style-regexp t
        evil-ex-search-case 'insensitive
        ;evil-ex-substitute-case nil
        evil-magic 'very-magic
        evil-want-C-g-bindings t
        evil-want-Y-yank-to-eol t
        evil-want-C-w-in-emacs-state t
        evil-want-keybinding nil
        evil-want-C-u-scroll t
        evil-transient-mouse-selection t
        evil-v$-excludes-newline t
        evil-want-fine-undo t)
  (evil-mode 1)
  (setq-default evil-symbol-word-search t)
  :general (:states           '(normal motion visual)
            "m"               'evil-next-visual-line
            "u"               'evil-previous-visual-line
            "n"               'evil-backward-char
            "h"               'evil-forward-char
            "N"               'evil-lookup
            "k"               'evil-ex-search-next
            "K"               'evil-ex-search-previous
            "j"               'undo-only
            "l"               'evil-set-marker
            "<S-tab>"         'evil-jump-backward
            "<S-iso-lefttab>" 'evil-jump-backward
            "<return>"        'evil-ex-nohighlight)
  (:states 'normal
   "g." (kbd "/ C-r \" <return> cgn C-@"))
  (:keymaps 'org-mode-map :states 'normal "o" (kbd "A <return>"))
  :custom (evil-undo-system 'undo-redo)
  :config (evil-select-search-module 'evil-search-module 'evil-search))

;; (use-package evil-collection
;;   :after evil
;;   :ensure t
;;   :config
;;   (with-eval-after-load 'dired (evil-collection-dired-setup)))

;(use-package targets
;  :straight (:host github :repo "noctuid/targets.el" :branch "master")
;  :config (targets-setup t :last-key "L" :next-key "N"))

(use-package evil-matchit
  :hook (prog-mode . evil-matchit-mode))

(use-package evil-anzu
  :init (global-anzu-mode))

(use-package evil-exchange
  :general (:states '(normal visual) "gx" 'evil-exchange))

;; (use-package evil-unimpaired
;;   :straight (:host github :repo "zmaas/evil-unimpaired")
;;   :hook (prog-mode . evil-unimpaired-mode))

(use-package evil-commentary
  :hook (prog-mode . evil-commentary-mode))

(use-package evil-lion)

;; (use-package evil-nerd-commenter
;;   :hook (prog-mode . evil-nerd))

(use-package evil-fringe-mark
  :hook (prog-mode . evil-fringe-mark-mode)
  :config (setq-default right-fringe-width 16
			evil-fringe-mark-side 'right-fringe))

(use-package evil-numbers
  :straight (:host github :repo "cofi/evil-numbers" :fork "juliapath/evil-numbers" :branch "master")
  :general (:states 'normal
                    "<kp-add>" 'evil-numbers/inc-at-pt
                    "<kp-subtract>" 'evil-numbers/dec-at-pt
                    "C-<kp-add>" 'evil-numbers/inc-at-pt-incremental
                    "C-<kp-subtract>" 'evil-numbers/dec-at-pt-incremental))

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
(use-package evil-textobj-entire
  :general
  (:keymaps '(evil-outer-text-objects-map evil-inner-text-objects-map)
   "e"     'evil-entire-entire-buffer))

(use-package evil-visualstar
  :config
  (global-evil-visualstar-mode))

(use-package so-long
  :init
  (global-so-long-mode)
  ;; Also related to long lines making emacs slow...
  (setq bidi-paragraph-direction 'left-to-right
	bidi-inhibit-bpa t))

(use-package hideshow
  :hook (prog-mode . hs-minor-mode)
  :config (add-to-list 'hs-special-modes-alist
                       `(ruby-mode
                         ,(rx (or "def" "class" "module" "do" "{" "[" "if" "else" "unless"))
                         ,(rx (or "}" "]" "end"))
                         ,(rx (or "#" "=begin"))
                         ruby-forward-sexp nil)))

(use-package which-key
  :init (setq which-key-popup-type 'minibuffer)
  :general (:keymaps 'override
                     "<f1>" 'which-key-show-top-level
                     "<f2>" 'which-key-show-major-mode)
  :config (which-key-mode))

(use-package smex)

(use-package ivy
  :init (ivy-mode 1)
  :general (:keymaps 'ivy-minibuffer-map
                     "<C-return>" 'ivy-immediate-done
                     "<f8>"       'ivy-rotate-preferred-builders
                     "C-d"        'ivy-done)
  :config (setq ivy-height 30))

(use-package counsel
  :general
  ("M-x"   'counsel-M-x
   "C-h v" 'counsel-describe-variable
   "C-h f" 'counsel-describe-function)
  (:keymaps '(motion override)
   "<f3>r" 'recentf-without-purpose
   "<f3>g" 'counsel-git
   "<f3>l" 'counsel-locate
   "<f3>L" 'find-file-literally
   "C-f"   'counsel-rg)
  (:keymaps 'minibuffer-local-map "C-r" 'counsel-minibuffer-history))

(use-package avy
  :config (setq avy-keys '(?n ?t ?i ?e ?o ?s ?h ?a ?g ?y ?l ?w ?r ?d)
		avy-background t)
  :general (:states '(normal visual motion) "SPC" 'avy-goto-char))

(use-package company
  :config
  (setq company-idle-delay 0.5
	comapny-show-numbers t
	company-tooltip-limit 10
	company-minimum-prefix-length 2
	company-dabbrev-downcase nil
	company-tooltip-align-annotations t
	company-tooltip-flip-when-above t)
  :hook
  (prog-mode . company-mode-on)
  (cider-repl-mode . company-mode-on)
  :general (:states 'insert "<f4>" 'company-dabbrev-code))

(use-package cl-lib
  :init
  (defun my-keyboard-escape-quit (fun &rest args)
    (cl-letf (((symbol-function 'one-window-p) (lambda (&rest _) t)))
      (apply fun args)))
  (advice-add 'keyboard-escape-quit :around #'my-keyboard-escape-quit))

(use-package window-purpose
  :general (:keymaps   'override
	    "£"        'switch-buffer-without-purpose
	    "C-£"      'purpose-switch-buffer-with-purpose
	    "<f3><f3>" 'find-file-without-purpose
	    "<f3>p"    'purpose-find-file-overload
	    "<f7>l"    'purpose-load-window-layout
	    "<f7>s"    'purpose-save-window-layout
	    "<f7>w"    'purpose-toggle-window-purpose-dedicated
	    "<f7>b"    'purpose-toggle-window-buffer-dedicated)
  :init (purpose-mode)
  :config
  (add-to-list 'purpose-user-mode-purposes '(js2-mode . js))
  (add-to-list 'purpose-user-mode-purposes '(clojure-mode . clj))
  (add-to-list 'purpose-user-mode-purposes '(clojurec-mode . clj))
  (add-to-list 'purpose-user-mode-purposes '(clojurescript-mode . clj))
  (add-to-list 'purpose-user-mode-purposes '(scss-mode . clj))
  (add-to-list 'purpose-user-mode-purposes '(graphql-mode . clj))
  (add-to-list 'purpose-user-mode-purposes '(dockerfile-mode . clj))
  (add-to-list 'purpose-user-mode-purposes '(markdown-mode . clj))
  (add-to-list 'purpose-user-mode-purposes '(cider-repl-mode . crm))
  (add-to-list 'purpose-user-mode-purposes '(magit-diff-mode . crm))
  (add-to-list 'purpose-user-mode-purposes '(adoc-mode . crm))
  (purpose-compile-user-configuration))

(defalias 'recentf-without-purpose
  (without-purpose-command #'counsel-recentf))

(defun avy-goto-asterisk ()
  "Use avy-goto-char with asterisk, for navigating magit log"
  (interactive) (avy-goto-char ?*))

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
	    "<C-tab>"  'ace-window)
  (:keymaps 'magit-file-section-map
            "<return>" 'magit-diff-visit-file-other-window)
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
  (setq magit-diff-refine-hunk t
        magit-revision-insert-related-refs nil))

(defun blame-local-keys ()
  (general-def
    :keymaps       'local
    :states        'normal
    "<S-return>"   'magit-show-commit))

(add-hook 'magit-blame-mode-hook 'blame-local-keys)

(use-package smerge-mode
  :hook
  (smerge-mode . (lambda () (evil-cleverparens-mode -1)))
  (smerge-mode . (lambda () (smartparens-mode -1))))

(defun auto-stage-untracked-file ()
  "Add an empty version of the currently-visited file to the index
iff it is in a git repo, but untracked."
  (when (and (fboundp 'magit-git-true)
             (condition-case nil
                 (magit-git-true "rev-parse" "--is-inside-work-tree")
               (error nil))
             (not (magit-file-tracked-p (buffer-file-name)))
             (not (equal "recentf" (buffer-name))) ; TODO find a better way to exclude these
             this-command ; nil for auto-save
             (y-or-n-p (format "Add (intent-to-add) %s to git index?" (buffer-name))))
    (magit-run-git "add" "--intent-to-add" "--" (buffer-file-name))))

(add-hook 'after-save-hook 'auto-stage-untracked-file)

(use-package restart-emacs
  :general ("<C-f5>" 'restart-emacs))

(use-package zoom
  :init
  (setq zoom-size '(110 . 0.6))
  (custom-set-variables
   '(zoom-mode t))
  :general (:keymaps 'override "<f5>" 'zoom-mode))

(use-package windmove
  :config (windmove-default-keybindings))

(use-package ace-window
  :general ("C-<tab>" 'ace-window)
  :config (setq aw-dispatch-always t
		aw-keys '(?n ?i ?h ?y ?l ?r ?t ?e ?s ?a ?g ?w ?d)))

(setq split-height-threshold 200)

(use-package centered-cursor-mode
  :general (:states 'normal "zz" 'centered-cursor-mode)
  :hook prog-mode cider-repl-mode magit-mode erc-mode org-mode
  :config
  (setq ccm-recenter-at-end-of-file t
        scroll-conservatively 101))

; (use-package centered-cursor-mode
;   :config
;   (setq ccm-recenter-at-end-of-file t)
;   (global-centered-cursor-mode)
;   :general ("<f8>" 'centered-cursor-mode))
; (setq scroll-conservatively 101)

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
(setq recentf-max-menu-items 400
      recentf-max-saved-items 400)
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
  (prog1 (get-buffer-create "**")
    (split-window-right)))
(setq initial-buffer-choice 'anon-note)

(use-package hi-lock
  :config (set-face-attribute 'hi-yellow nil :background "#4e5565" :foreground "#abb2bf"))
;; Formerly hl-thing or highlight-thing mode
(use-package idle-highlight-mode
  :custom
  (idle-highlight-exclude-point t)
  (idle-highlight-exceptions-face nil)
  :config (setq idle-highlight-idle-time 0.3
                idle-highlight-visible-buffers t)
  :hook ((prog-mode text-mode) . idle-highlight-mode)
  :general (:states 'normal "C-8" 'idle-highlight-mode))

(use-package hl-todo
  :config (global-hl-todo-mode))

(use-package midnight)

(defun org-local-setup ()
  (visual-line-mode 1)
  (flyspell-mode 1)
  (prettify-symbols-mode 1)
  (general-def
    :keymaps     'local
    :states      'insert
    "<return>"   'org-meta-return
    "<S-return>" 'org-return
    "<C-return>" 'org-insert-heading-respect-content))

(use-package org
  :hook
  (org-mode . org-local-setup)
  (org-after-todo-state-change . (lambda () (when (equal "DONE" org-state)
                                              (call-interactively #'org-archive-to-archive-sibling))))
  :config (setq org-agenda-files '("~/Documents/Work"
                                   "~/org/work-tasks.org")
                org-todo-keywords '((sequence "NEXT(n)" "WAITING(w)" "THINK(t)" "LATER(l)" "DONE(d)"))
                org-agenda-sorting-strategy '((agenda habit-down time-up priority-down category-keep)
                                              (todo priority-down todo-state-up category-keep)
                                              (tags priority-down category-keep)
                                              (search category-keep))))

(defun insert-current-date ()
  "Insert the current date in format YYYY-MM-DD."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))
(general-def '(motion insert) "\C-c TAB" 'insert-current-date)


(defun first-non-blank-org (&rest args)
  (when (and (derived-mode-p 'org-mode)
             (looking-at "\\*+"))
    (skip-chars-forward "* ")))
(advice-add 'evil-first-non-blank :after #'first-non-blank-org)

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

(use-package diff-hl
  :init (global-diff-hl-mode))

(defun conditionally-enable-smartparens-mode ()
  "Enable `smartparens-mode' in the minibuffer, during `eval-expression'."
  (when (eq this-command 'eval-expression)
    (smartparens-mode 1)
    (sp-pair "'" nil :actions :rem)))

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
  :init (setq evil-cleverparens-use-regular-insert t)
  :hook
  (prog-mode . evil-cleverparens-mode)
  (cider-repl-mode . evil-cleverparens-mode)
  :custom
  (evil-cleverparens-use-additional-bindings nil)
  (evil-cleverparens-use-additional-movement-keys nil)
  :general (:states  '(normal motion visual)
            :keymaps '(global evil-cleverparens-mode-map)
            "{"      'evil-backward-paragraph
            "}"      'evil-forward-paragraph
            "M-l"    nil
            "x"      'evil-delete-char
            "X"      'fixup-whitespace
            "("      'evil-cp-backward-up-sexp
            ")"      'evil-cp-up-sexp
            "M-w"    'evil-forward-WORD-begin
            "<f9>"   'evil-cleverparens-mode)
           (:states  'motion
            "]]"     'evil-cp-next-closing
            "]["     'evil-cp-next-opening
            "[["     'evil-cp-previous-opening
            "[]"     'evil-cp-previous-closing)
   :config (evil-define-key 'normal evil-cleverparens-mode-map "s" nil "S" nil))
(general-def "M-l" 'linum-mode)

;; (use-package lispy
;;   ;; TODO - slurp/barf bindings
;;   :hook
;;   (prog-mode . lispy-mode)
;;   (cider-repl-mode . lispy-mode)
;;   :config
;;   (lispy-set-key-theme '(lispy c-digits)))

;; (use-package lispyville
;;   :hook
;;   (prog-mode . lispyville-mode)
;;   (cider-repl-mode . lispyville-mode)
;;   :config
;;   (lispyville-set-key-theme '(operators c-w additional))
;;   :general (:states '(normal visual)
;;             :keymaps '(global lispyville-mode-map)
;;             "x"      'evil-delete-char
;;             "X"      'fixup-whitespace))

(use-package ispell
  :config
  (setq ispell-program-name "hunspell")
  (setenv "DICTPATH" "/usr/share/hunspell/en_GB"))

(use-package flycheck
  :init (global-flycheck-mode)
  :config (setq flycheck-indication-mode nil)
  :general (:states 'motion
                    "]c" 'flycheck-next-error ;; TODO set :repeat type to motion
                                              ;; TODO test on 1st line of buffer
                    "[c" 'flycheck-previous-error))
(use-package flycheck-clj-kondo)
(dolist (checker '(clj-kondo-clj clj-kondo-cljs clj-kondo-cljc clj-kondo-edn))
  (setq flycheck-checkers (cons checker (delq checker flycheck-checkers))))

(defun defx* (vars)
  (let ((len (length vars))
        (cvar (car-safe vars)))
    (cond
     ((= len 1) (insert (format "(def %s %s)" cvar cvar)))
     ((< 1 len)
      (insert (format "(def %s %s)\n" cvar cvar))
      (indent-according-to-mode)
      (defx* (cdr vars))))))

(defun defx (input)
  (interactive "svars: ")
  (defx* (split-string input)))

(use-package clojure-mode
  :config (require 'flycheck-clj-kondo)
  :general
  (:keymaps 'clojure-mode-map
   "C-j"    'cider-jack-in-clj&cljs
   "C-S-r"  'lsp-rename
   "ð"      'defx
   "C-ð"    'defx))
(defun evil-set-jump-nullary (&rest _) (evil-set-jump))
(advice-add 'cider-find-var :before #'evil-set-jump-nullary)
(use-package clojure-mode-extra-font-locking)
(use-package cider
  :config
  (add-to-list 'exec-path "~/bin/")
  (setq cider-show-error-buffer nil
        cider-prompt-for-symbol nil
        nrepl-hide-special-buffers t
        cider-repl-require-ns-on-set t
        cider-font-lock-dynamically '(macro core function deprecated var)
        cider-format-code-options
        '(("remove-multiple-non-indenting-spaces?" t)))
  :general
  (:keymaps 'cider-mode-map
   "C-n"    'cider-repl-set-ns
   "C-→"    'evil-cider-inspect
   "M-i"    'cider-inspect-last-sexp
   "→"      'evil-cider-inspect)
  (:keymaps 'cider-repl-mode-map
   :states '(normal insert)
   "C-:" 'clojure-toggle-keyword-string
   "¶"   'cider-repl-switch-to-other
   "C-c C-l" 'cider-repl-clear-buffer
   "M-i" 'cider-inspect
   "C-→"    'evil-cider-inspect
   "M-i"    'cider-inspect-last-sexp
   "→"      'evil-cider-inspect)
  (:keymaps 'cider-stacktrace-mode-map
   :states 'normal
   "q" 'cider-popup-buffer-quit-function)
  (:keymaps 'cider-inspector-mode-map
   :states 'normal
   "<return>"        'cider-inspector-operate-on-point
   "l"               'cider-inspector-pop
   "g"               'cider-inspector-refresh
   "q"               'cider-popup-buffer-quit-function
   "<S-iso-lefttab>" 'cider-inspector-previous-inspectable-object
   "SPC"             'cider-inspector-next-page
   "M-SPC"           'cider-inspector-prev-page))

(evil-define-operator evil-cider-eval (beg end)
  "Evalulate the text region moved over by an evil motion."
  :motion evil-cp-a-defun
  (cider-eval-region beg end))

(evil-define-operator evil-cider-inspect (beg end)
  "Inspect the text region moved over by an evil motion."
  (let ((expr (buffer-substring-no-properties beg end)))
    (cider-inspect-expr expr (cider-current-ns))))

(use-package clj-refactor)

(use-package adoc-mode
  :init (setq auto-mode-alist
              (append auto-mode-alist '(("\\.txt\\'" . adoc-mode)
                                        ("\\.adoc\\'" . adoc-mode))))
  :hook (adoc-mode . flyspell-mode))

(use-package css-mode
  :init (setq css-indent-offset 2
	      css-fontify-colors nil))

(use-package scss-mode
  :init (add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode)))

(use-package lsp-mode)
(use-package lsp-java :config (add-hook 'java-mode-hook 'lsp))
(use-package lsp-treemacs)

;; (use-package lsp-mode
;;   :config
;;   (setq lsp-enable-snippet nil
;;         lsp-enable-indentation nil
;;         lsp-file-watch-threshold nil)
;;   (dolist (m '(clojure-mode
;;                clojurec-mode
;;                clojurescript-mode
;;                clojurex-mode))
;;     (add-to-list 'lsp-language-id-configuration `(,m . "clojure")))
;;   :hook
;;   (clojure-mode . lsp)
;;   (clojurec-mode . lsp)
;;   (clojurescript-mode . lsp))

(use-package csv-mode)

(use-package dockerfile-mode
  :init (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

(use-package auctex
  ;; Outside of emacs: sudo pacman -Syu texlive-most
  ;; Compile tex file with C-c C-c and choose Latex
  ;; Open PDF in another buffer and do M-x auto-revert-mode
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
  :keymaps 'override
  "C-c ESC"    'ignore
  "M-£"        'bury-buffer
  "C-M-£"      'kill-some-buffers
  "<f3>i"      'open-init
  "<f3>k"      'kill-this-buffer
  "<f3>m"      'magit-find-file
  "C-/"        'undo-fu-only-undo
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
  :general (:states     'normal
	    :keymaps    'dired-mode-map
	    "<return>"  'dired-find-file
	    "<mouse-1>" 'dired-find-file
	    "w"         'wdired-change-to-wdired-mode
	    "m"         'dired-next-line
	    "u"         'dired-previous-line
	    "n"         'evil-backward-char
	    "h"         'evil-forward-char
	    "L"         'dired-unmark
	    "C-S-l"     'dired-do-load
            "I"         'dired-kill-subdir
	    "<SPC>"     'avy-goto-line)
  :custom ((dired-listing-switches "-aBgh --group-directories-first")))
(advice-add 'dired-kill-subdir :around #'all-the-icons-dired--refresh-advice)
(use-package all-the-icons-dired
  :after (dired)
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-narrow)

(defun dired-local-keys ()
  (general-def
    :keymaps        'local
    :states         'normal
    "l"             'dired-mark
    "g"             'revert-buffer))
(add-hook 'dired-mode-hook 'dired-local-keys)

(use-package async
  :config (dired-async-mode 1))

(general-def
  :states '(normal motion visual)
  "M-*"   (lambda (&optional count) (interactive "p") (evil-search-word-forward count nil))
  "M-#"   (lambda (&optional count) (interactive "p") (evil-search-word-backward count nil)))

(evil-define-operator evil-narrow-region (beg end)
  "Indirectly narrow the region from BEG to END."
  (interactive "<r>")
  (evil-normal-state)
  (narrow-to-region beg end))
(general-def
  :states '(normal visual)
  "M-n" 'evil-narrow-region
  "M-N" 'widen)

;; Differentiate C-m from RET
;; (when (display-graphic-p)
;;   (general-def input-decode-map [?\C-m] [C-m]))

(defun repl-local-keys ()
  (general-def
    :keymaps          'local
    :states           '(normal insert visual)
    "<C-up>"          'cider-repl-backward-input
    "<C-down>"        'cider-repl-forward-input
    "<C-return>"      'cider-repl-newline-and-indent))

(add-hook 'cider-repl-mode-hook 'repl-local-keys)

(defun ivy-occur-local-keys ()
  (general-def
    :keymaps   'local
    :states    '(normal motion visual)
    "<return>" 'ivy-occur-press-and-switch))

(add-hook 'ivy-occur-grep-mode-hook 'ivy-occur-local-keys)
(use-package wgrep
  :custom (wgrep-auto-save-buffer t))

;(general-unbind '(normal motion) "C-e")
(general-unbind :keymaps '(evil-mc-key-map evil-normal-state-map) "C-n")

(general-def
  :keymaps 'cider-mode-map
  ;"C-e"    'evil-cider-eval
  "C-n"    'cider-repl-set-ns
  "M-c"    'cider-pprint-eval-defun-to-comment
  "M-C"    'cider-pprint-eval-last-sexp-to-comment)

(defun collapse-comments ()
  (interactive)
  (evil-ex "global/\\(comment/normal zc"))

(setq truncate-partial-width-windows t)
(defun truncate-partial-width-windows-50 ()
  (interactive)
  (if (eq truncate-partial-width-windows t)
      (setq truncate-partial-width-windows 50)
    (setq truncate-partial-width-windows t)))
(general-def "<f6>" 'truncate-partial-width-windows-50)
(general-def "S-<f6>" 'visual-line-mode)

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

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.mdx\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

(use-package treemacs
  :general (:keymaps 'normal "C-t" 'treemacs))

;; (when-let ((bash-path (executable-find "bash")))
;;   (setenv "SHELL" bash-path)
;;   (setq-default explicit-shell-file-name bash-path
;;                 shell-file-name bash-path))

(evil-define-operator sort-graph-fields (beg end)
  (interactive "<r>")
  (sort-regexp-fields nil "[[:graph:]]+" "" beg end))

(evil-define-operator sort-graph-fields-reverse (beg end)
  (interactive "<r>")
  (sort-regexp-fields t "[[:graph:]]+" "" beg end))

(evil-define-operator evil-sort-lines (beg end)
  (interactive "<r>")
  (sort-lines nil beg end))

(evil-define-operator evil-sort-lines-reverse (beg end)
  (interactive "<r>")
  (sort-lines t beg end))

(general-def :states 'normal "gs" 'sort-graph-fields)
(general-def :states 'normal "gS" 'sort-graph-fields-reverse)
(general-def :states 'normal "gl" 'evil-sort-lines)
(general-def :states 'normal "gL" 'evil-sort-lines-reverse)

(use-package yaml-mode)

(evil-define-command evil-delete-registers (registers &optional force)
  (interactive "<a><!>")
  (when (and force registers) (user-error "Invlaid input"))
  (let ((reg-chars (if force
                       (number-sequence ?a ?z)
                     (evil--parse-delmarks (remove ?\s (append registers nil))))))
    (dolist (reg-char reg-chars)
      (set-register reg-char nil))))
(evil-ex-define-cmd "delr[egisters]" 'evil-delete-registers)

(use-package pcre2el)

(defun minibuffer-evil ()
  (interactive)
  (evil-local-mode 1)
  (evil-initialize-state))
(define-key evil-ex-completion-map "\C-f" 'minibuffer-evil)

(evil-define-command evil-use-black-hole-register ()
  "Use `_' register."
  (interactive)
  (setq this-command 'evil-use-register
        evil-this-register ?_))

(general-def :states '(normal visual) :keymaps 'evil-cleverparens-mode-map
  "-" #'evil-use-black-hole-register)

(defun improve-c-o-e (&rest _)
  "Make e and E reach past the end of the word/WORD after C-o."
  (when (and (eq 'evil-execute-in-normal-state last-command)
             (not (evil-operator-state-p)))
    (evil-forward-char)))
(advice-add 'evil-forward-word-end :after #'improve-c-o-e)
(advice-add 'evil-cp-forward-symbol-end :after #'improve-c-o-e)

(defun js-console-log (var)
  (interactive "svar: ")
  (insert (format "(js/console.log \"%s: \" %s)" var var)))
(general-def :states '(normal insert) :keymaps 'smartparens-mode-map
  "M-j" #'js-console-log)

(use-package restclient
  :init (add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode)))

(use-package inf-ruby
  :hook (ruby-mode . inf-ruby-minor-mode))

(use-package flymake-ruby
  :hook (ruby-mode . flymake-ruby-load))

(use-package json-navigator)

;; Not working
(use-package yari
  :general (:states 'normal :keymaps 'ruby-mode "N" 'yari))

(use-package kotlin-mode)

(use-package typescript-mode)

(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :config (pdf-tools-install :no-query)
  :hook (pdf-view-mode . pdf-view-fit-page-to-window))

(use-package digit-groups
  :hook
  (prog-mode . digit-groups-mode)
  (cider-repl-mode . digit-groups-mode))

;; For ZMK keymaps
(font-lock-add-keywords 'c-mode
                        '(("\\(&\\sw+\\)"
                           (1 'font-lock-keyword-face))))

(use-package rust-mode)

;; (use-package explain-pause-mode
;;   :straight (explain-pause-mode :type git :host github :repo "lastquestion/explain-pause-mode")
;;   :config
;;   (explain-pause-mode))

;; Useful for testing
;; emacs -Q -L "path/to/evil" -l "path/to/evil.el" -l "path/to/evil-unimpaired.el" --eval "(evil-mode 1)" --eval "(evil-unimpaired-mode)"

;; When sbcl is installed
;; (setq inferior-lisp-program "/usr/bin/sbcl")

