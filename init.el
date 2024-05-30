(defvar comp-deferred-compliation)
(setq comp-deferred-compilation t)

(setq package-enable-at-startup nil)

;; --------------- Power User Stuff ---------------

(setq inhibit-startup-message t)
(put 'set-goal-column 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'eval-expression 'disabled nil)
(put 'erase-buffer 'disabled nil)

(setq-default show-trailing-whitespace nil)
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
;;(setq backup-inhibited t)
(column-number-mode 'true)
(setq compile-command "make -j")
(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t)

;; --------------- Theme ---------------

(load-theme 'wombat t)

;; --------------- Generic key binding ---------------

(global-set-key "\M-g"   'goto-line)
(global-set-key "\M-r"   'revert-buffer)
;;(global-set-key "\M-o"   'tags-search)
;;(global-set-key [f7]     'previous-error)
;;(global-set-key [f8]     'next-error)
(global-set-key "\C-ce"  'ediff-buffers)
(global-set-key "\C-c "  'cc-find-other-file)
(global-set-key "\C-ca"  'cc-append-to-line)
(global-set-key "\C-cc"  'cc-chomp-lines)
;;(global-set-key "\C-cr"  'cc-chomp-lines-regexp)
(global-set-key "\M-*"   'rtags-location-stack-back)
(global-set-key "\M-@"   'er/expand-region)
(global-set-key "\M-#"   'mc/mark-next-like-this)
(global-set-key (kbd "M-C-c") 'mc/edit-lines)
(global-set-key "\M-n"   'next-error)
(global-set-key "\M-p"   'previous-error)
(global-set-key "\C-s"   'swiper)

(global-set-key "\M-."   'rtags-find-symbol-at-point)

;; --------------- End of key bindings ---------------

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

(setq package-enable-at-startup nil)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

(menu-bar-mode -1)
(unless (and (display-graphic-p) (eq system-type 'darwin))
    (push '(menu-bar-lines . 0) default-frame-alist))
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; max memory available for gc on startup
(defvar me/gc-cons-threshold 16777216)
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold me/gc-cons-threshold
                  gc-cons-percentage 0.1)))

;; max memory available for gc when opening minibuffer
(defun me/defer-garbage-collection-h ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun me/restore-garbage-collection-h ()(setq site-run-file nil)
  ;; Defer it so that commands launched immediately after will enjoy the
  ;; benefits.
  (run-at-time
   1 nil (lambda () (setq gc-cons-threshold me/gc-cons-threshold))))

(add-hook 'minibuffer-setup-hook #'me/defer-garbage-collection-h)
(add-hook 'minibuffer-exit-hook #'me/restore-garbage-collection-h)
(setq garbage-collection-messages t)

(defvar me/-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist me/-file-name-handler-alist)))

(setq site-run-file nil)

(setq inhibit-compacting-font-caches t)


(when (boundp 'read-process-output-max)
  ;; 1MB in bytes, default 4096 bytes
  (setq read-process-output-max 1048576))


(setq straight-use-package-by-default t
      use-package-always-defer t
      straight-cache-autoloads t
      straight-vc-git-default-clone-depth 1
      straight-check-for-modifications '(find-when-checking)
      package-enable-at-startup nil
      vc-follow-symlinks t)

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
(setq vc-follow-symlinks 'ask) ; restore default

(require 'straight-x)

(straight-use-package 'use-package)

(use-package esup
  :demand t
  :commands esup)

(use-package benchmark-init
  :demand t
  :straight (:host github :repo "kekeimiku/benchmark-init-el")
  :hook (after-init . benchmark-init/deactivate))

(add-hook
 'emacs-startup-hook
 (lambda ()
   (message "Emacs ready in %s with %d garbage collections."
            (format
             "%.2f seconds"
             (float-time
              (time-subtract after-init-time before-init-time)))
            gcs-done)))

(use-package gcmh
  :demand t
  :config
  (gcmh-mode 1))

(provide 'early-init)

(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

(use-package yasnippet
  :ensure t)

(use-package magit
  :ensure t)

(use-package projectile
  :ensure t)

(use-package swiper
  :ensure t)

(use-package avy
  :ensure t)

(use-package key-chord
   :ensure t
   :config
   (key-chord-mode 1)
   (key-chord-define-global "jl" 'avy-goto-line)
   (key-chord-define-global "jk" 'avy-goto-char)
   (defvar key-chord-tips
     '("Press <jl> quickly to jump to a visible line."
       "Press <jk> quickly to jump to a visible character.")))

(use-package rtags
  :ensure t)

;;(use-package savehist
;;  :demand t
;;  :config
;;  (savehist-mode))

(use-package vertico
  :demand t
  :custom
  (vertico-resize t)
  (vertico-cycle t)
  :config
  (vertico-mode))

;;(use-package icomplete
;;  :custom
;;  (read-file-name-completion-ignore-case t)
;;  (read-buffer-completion-ignore-case t)
;;  (completion-ignore-case t)
;;
;;  (completion-category-defaults nil)
;;  (completion-category-overrides
;;   '((file (styles basic partial-completion))))
;;
;;  (completion-group t)
;;  (completions-group-format
;;        (concat
;;         (propertize "    " 'face 'completions-group-separator)
;;         (propertize " %s " 'face 'completions-group-title)
;;         (propertize " " 'face 'completions-group-separator
;;                     'display '(space :align-to right)))))

(use-package orderless
  :demand t
  :custom
  (completion-styles '(orderless))
  :config
  (defun prefix-if-tilde (pattern _index _total)
    (when (string-suffix-p "~" pattern)
      `(orderless-prefixes . ,(substring pattern 0 -1))))

  (defun regexp-if-slash (pattern _index _total)
    (when (string-prefix-p "/" pattern)
      `(orderless-regexp . ,(substring pattern 1))))

  (defun literal-if-equal (pattern _index _total)
    (when (string-suffix-p "=" pattern)
      `(orderless-literal . ,(substring pattern 0 -1))))

  (defun without-if-bang (pattern _index _total)
    (cond
     ((equal "!" pattern)
      '(orderless-literal . ""))
     ((string-prefix-p "!" pattern)
      `(orderless-without-literal . ,(substring pattern 1)))))

  (setq orderless-matching-styles '(orderless-flex))
  (setq orderless-style-dispatchers
        '(prefix-if-tilde
          regexp-if-slash
          literal-if-equal
          without-if-bang)))

(use-package consult
;;  (leader-def
;;    "ff" 'find-file
;;    "fr" 'consult-recent-file
;;    "bb" 'consult-buffer
;;    "tc" 'consult-theme
;;    "/"  'consult-ripgrep
;;    "g/" 'consult-git-grep)
  :custom
  (consult-project-root-function #'projectile-project-root)
  (consult-narrow-key "<"))

;; (use-package consult-projectile
;;   :after consult projectile
;;   :demand t
;;   :straight (consult-projectile :type git :host gitlab :repo "OlMon/consult-projectile" :branch "master")
;;   (leader-def
;;     "pp" 'consult-projectile))

;; (use-package consult-lsp
;;   :straight (consult-lsp :type git :host github :repo "gagbo/consult-lsp" :protocol ssh)
;;   :commands (consult-lsp-symbols consult-lsp-diagnostics consult-lsp-file-symbols)
;;   :config (define-key lsp-mode-map [remap xref-find-apropos] #'consult-lsp-symbols))

(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))

(defun me/disable-global-hl-line-mode ()
  (global-hl-line-mode -1))

(use-package vterm
  :straight nil
  :commands vterm vterm-other-window
  :hook (vterm-mode . #'me/disable-global-hl-line-mode)
  :custom
  (vterm-term-environment-variable "eterm-color")
  :config
  (remove-hook 'vterm-mode-hook 'vterm))

;; (use-package multi-vterm
;;   :commands
;;   multi-vterm
;;   multi-vterm-next
;;   multi-vterm-prev
;;   multi-vterm-dedicated-toggle
;;   multi-vterm-project)
;; (leader-def "pt" 'multi-vterm-dedicated-toggle)

(use-package pcmpl-args)
