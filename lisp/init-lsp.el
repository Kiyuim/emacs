;;; init-lsp.el -*- lexical-binding: t no-byte-compile: t -*-
;; Author: Kiyu
;; Github: https://github.com/Kiyuim
;;; Commentary:
;; (c) Kiyu, 2024-
;;; Code:

(use-package eglot
	:hook (prog-mode . eglot-ensure)
	:bind (:map eglot-mode-map
              ("C-c l a" . eglot-code-actions)
              ("C-c l r" . eglot-rename)
              ("C-c l o" . eglot-code-action-organize-imports)
              ("C-c l f" . eglot-format)
              ("C-c l d" . eldoc)
              ("s-<return>" . eglot-code-actions))
	:config
	(setq eglot-send-changes-idle-time 0.2)
	(setq eldoc-echo-area-use-multiline-p nil)
	(setq eglot-connect-timeout 120))

(use-package consult-eglot
	:ensure t
	:defer t)

(provide 'init-lsp)

;;; init-lsp.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; END: