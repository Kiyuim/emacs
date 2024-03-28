;;; init-base.el -*- lexical-binding: t no-byte-compile: t -*-
;; Author: Kiyu
;; Github: https://github.com/Kiyuim
;;; Commentary:
;; (c) Kiyu, 2024-
;;; Code:

(global-set-key (kbd "C-h C-f") 'find-function)
(global-set-key (kbd "C-h C-v") 'find-variable)
(global-set-key (kbd "C-h C-k") 'find-function-on-key)

(global-set-key (kbd "TAB") 'self-insert-command)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq backward-delete-char-untabify-method nil)

;; mimic macos keybindgs
(setq mac-option-modifier 'meta
      mac-command-modifier 'super)
(progn
    (global-set-key (kbd "s-x") 'kill-region)
    (global-set-key (kbd "s-c") 'kill-ring-save)
    (global-set-key (kbd "s-v") 'yank)
    (global-set-key (kbd "s-z") 'undo)
    (global-set-key (kbd "s-l") 'goto-line)
    ;; (global-set-key (kbd "s-q") 'save-buffers-kill-emacs)
    (global-set-key (kbd "s-a") 'mark-whole-buffer)
    (global-set-key (kbd "s-s") 'save-buffer))

(display-battery-mode 1)

(fset 'yes-or-no-p 'y-or-n-p)

;;(setq ring-bell-function 'ignore)

(setq default-directory "~")

;; No backup files
(setq make-backup-files nil
      auto-save-default nil)

;; No lock files
(setq create-lockfiles nil)

;; Always load the newest file
(setq load-prefer-newer t)

;; Cutting and pasting use primary/clipboard
(setq select-enable-primary t
        select-enable-clipboard t)

;; auto revert
;; `global-auto-revert-mode' is provided by autorevert.el (builtin)
(add-hook 'after-init-hook 'global-auto-revert-mode)
;
; auto save to the visited file (provided by `files.el')
(add-hook 'after-init-hook 'auto-save-visited-mode)

(add-hook 'prog-mode-hook 'electric-pair-mode)

;; Delete Behavior
;; `delete-selection-mode' is provided by delsel.el (builtin)
(add-hook 'after-init-hook 'delete-selection-mode)

;; visual-line-mode
(add-hook 'after-init-hook 'global-visual-line-mode)

;; pixel-scroll-precise-mode
(add-hook 'after-init-hook 'pixel-scroll-precision-mode)

;; Enable the disabled narrow commands
(put 'narrow-to-defun  'disabled nil)
(put 'narrow-to-page   'disabled nil)
(put 'narrow-to-region 'disabled nil)

(use-package restart-emacs
	:ensure t
	:commands (restart-emacs))  

(unless emacs-major-version 29  
    (use-package iscroll
        :diminish
        :hook (image-mode . iscroll-mode)))

(use-package paren
    :ensure nil
    :hook (after-init . show-paren-mode)
    :custom
    (show-paren-when-point-inside-paren t)
    (show-paren-when-point-in-periphery t))

(use-package smartparens
    :ensure t
    :init
    (smartparens-global-mode t)
    (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
    (sp-local-pair 'emacs-lisp-mode "`" nil :actions nil)
    (sp-local-pair 'lisp-interaction-mode "'" nil :actions nil)
    :config
        (sp-with-modes
            '(c++-mode objc-mode c-mode)
            (sp-local-pair "{" nil :post-handlers '(:add ("||\n[i]" "RET")))))

(use-package highlight-parentheses
    :ensure t)

(use-package flymake
    :ensure nil
    :diminish (flymake " Flym.")
    :hook (prog-mode . flymake-mode)
    :bind (("M-n" . flymake-goto-next-error)
           ("M-p" . flymake-goto-prev-error)
           ("C-c ! l" . flymake-show-buffer-diagnostics)))

;; (use-package undo-tree
;;     :ensure t
;;     :init (global-undo-tree-mode)
;;     :custom
;;     (undo-tree-auto-save-history nil))

(use-package repeat
    :ensure nil
    :custom
    (repeat-mode t)
    (repeat-exit-timeout 3)
    (repeat-exit-key (kbd "RET")))

(use-package saveplace
    :ensure nil
    :hook (after-init . save-place-mode)
    :init
    (setq save-place-file (expand-file-name ".cache/places" user-emacs-directory)))

(use-package savehist
	:ensure nil
	:hook (after-init . savehist-mode)
	:init (setq enable-recursive-minibuffers t ; Allow commands in minibuffers
	history-length 1000
	savehist-additional-variables '(mark-ring
									global-mark-ring
									search-ring
									regexp-search-ring
									extended-command-history)
	 savehist-autosave-interval 300))

;; Server mode.
;; Use emacsclient to connect
(use-package server
    :ensure nil
    :hook (after-init . server-mode))

;; (use-package hungry-delete
;;     :ensure t
;;     :init
;;     (global-hungry-delete-mode))

;;Recently opened files
(use-package recentf
    :ensure nil
    :hook (after-init . recentf-mode)
    :custom
    (recentf-max-saved-items 300)
    (recentf-auto-cleanup 'never)
    (recentf-exclude '(;; Folders on MacOS start
                       "^/private/tmp/"
                       "^/var/folders/"
                       ;; Folders on MacOS end
                       "^/tmp/"
                       "/ssh\\(x\\)?:"
                       "/su\\(do\\)?:"
                       "^/usr/include/"
                       "/TAGS\\'"
                       "COMMIT_EDITMSG\\'")))
    ;; :bind (("C-c C-r" .#'recentf-open)))


(use-package visual-fill-column
	:ensure t
	:init
	(setq visual-fill-column-width 110
    	visual-fill-column-center-text t))

;; move-dup, move/copy line or region
(use-package move-dup
    :ensure t
    :hook (after-init . global-move-dup-mode))

(use-package valign
	:ensure t
	:hook ((markdown-mode org-mode) . valign-mode))

(use-package pinyinlib
    :after orderless
    :autoload pinyinlib-build-regexp-string
    :init
    (defun completion--regex-pinyin (str)
        (orderless-regexp (pinyinlib-build-regexp-string str)))
    (add-to-list 'orderless-matching-styles 'completion--regex-pinyin))
(use-package pyim
  :ensure t
  :commands (pyim-cregexp-build)
  :init
  (defun eh-orderless-regexp (orig_func component)
    (let ((result (funcall orig_func component)))
      (pyim-cregexp-build result))))

(use-package ispell-minor-mode
    :ensure nil
    :config
    (advice-add 'ispell-lookup-words :around
        (lambda (orig &rest args)
            (shut-up (apply orig args)))))

(use-package flyspell-correct
    :ensure t
    :init)

(use-package ispell
    :ensure nil
    :init
    (when (eq system-type 'windows-nt)
        (setq ispell-program-name "aspell"))
    (dolist (hook '(text-mode-hook))
        (add-hook hook (lambda () (flyspell-mode 1))))
    (ispell-change-dictionary "american" t))

(use-package quickrun
 	:ensure t
 	:commands (quickrun))

;; MacOS specific
(use-package exec-path-from-shell
    :ensure t
    :when (eq system-type 'darwin)
    :hook (after-init . exec-path-from-shell-initialize))

(provide 'init-base)

;;; init-base.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; END:
