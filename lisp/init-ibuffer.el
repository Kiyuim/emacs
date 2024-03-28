;;; init-ibuffer.el -*- lexical-binding: t no-byte-compile: t -*-
;; Author: Kiyu
;; Github: https://github.com/Kiyuim
;;; Commentary:
;; (c) Kiyu, 2024-
;;; Code:

(use-package ibuffer
	:ensure nil
	:bind ("C-x C-b" . ibuffer)
	:init (setq ibuffer-filter-group-name-face '(:inherit (font-lock-string-face bold))))

;; Display icons for buffers
(use-package nerd-icons-ibuffer
	:ensure t
	:hook (ibuffer-mode . nerd-icons-ibuffer-mode))

;; Group ibuffer's list by project
(use-package ibuffer-project
	:hook (ibuffer . (lambda ()
                    	"Group ibuffer's list by project."
                    	(setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
                    	(unless (eq ibuffer-sorting-mode 'project-file-relative)
                    		(ibuffer-do-sort-by-project-file-relative))))
	:init (setq ibuffer-project-use-cache t))

(provide 'init-ibuffer)

;;; init-ibuffer.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; END: