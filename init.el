;;; init.el --- the entry of emacs config -*- lexical-binding: t -*-
;; Author: Kiyu
;; Github: https://github.com/Kiyuim
;;; Commentary:
;; (c) Kiyu, 2024-
;;; Code:

(let ((dir (locate-user-emacs-file "lisp")))
    (add-to-list 'load-path (file-name-as-directory dir))
    (add-to-list 'load-path (file-name-as-directory (expand-file-name "language" dir))))
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file 'no-error 'no-message)

(require 'init-packages)
(require 'init-base)
(require 'init-ibuffer)
(require 'init-ui)
(require 'init-treesit)
(require 'init-lsp)
(require 'init-completion)
(require 'init-treemacs)
(require 'temp)

;;; init.el ends here
;;; Local Variables:
;; coding: utf-8
;; byte-compile-warnings: (not unresolved obsolete)
;; End:
