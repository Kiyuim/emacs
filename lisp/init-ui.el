;;; init-ui.el -*- lexical-binding: t no-byte-compile: t -*-
;; Author: Kiyu
;; Github: https://github.com/Kiyuim
;;; Commentary:
;; (c) Kiyu, 2024-
;;; Code:

(set-face-attribute 'default nil :height 160)
(setq-default cursor-type 'bar) 

(add-hook 'prog-mode-hook 'hs-minor-mode)
(add-hook 'prog-mode-hook 'prettify-symbols-mode)

(use-package hl-line
    :when (display-graphic-p)
    :hook (prog-mode . hl-line-mode))

(use-package display-line-numbers
    :ensure nil
    :hook ((prog-mode yaml-mode conf-mode org-mode) . display-line-numbers-mode)
    :init 
    (setq global-display-line-numbers t)
    (setq column-number-mode t)
    (setq display-line-numbers-type 'relative))
(add-hook 'prog-mode-hook 'column-number-mode)

(use-package nerd-icons
    :ensure t
    :when (display-graphic-p)
    :demand t)

(use-package doom-themes
    :ensure t
    :init
    (if (display-graphic-p)
        (load-theme 'doom-one t)
        (load-theme 'doom-tomorrow-night t)))

(use-package dashboard
    :ensure t
    :init
    (setq dashboard-navigator-buttons `(((,(if (fboundp 'nerd-icons-octicon) (nerd-icons-octicon "nf-oct-mark_github") "★")
                                        "GitHub" "Browse" (lambda (&rest _) (browse-url homepage-url)))
                                       (,(if (fboundp 'nerd-icons-octicon) (nerd-icons-octicon "nf-oct-mail") "✉")
                                        "Mail" "Show mail" (lambda (&rest _) (browse-url mail-url)))
                                       (,(if (fboundp 'nerd-icons-octicon) (nerd-icons-octicon "nf-oct-download") "♺")
                                        "Upgrade" "Upgrade packages synchronously" (lambda (&rest _) (package-upgrade-all nil)) success))))
    (dashboard-setup-startup-hook)
    :config
    (defconst homepage-url "https://github.com/kiyuim")
    (defconst mail-url "https://outlook.live.com/mail/")
    :custom
    (dashboard-startup-banner 'logo)
    (dashboard-set-heading-icons t)
    (dashboard-set-file-icons t)
    (dashboard-center-content t)
    (dashboard-items '((projects  . 6)
                     (recents   . 10)))
    (dashboard-startupify-list '(dashboard-insert-banner
                               dashboard-insert-newline
                               dashboard-insert-banner-title
                               dashboard-insert-newline
                               dashboard-insert-navigator
                               dashboard-insert-newline
                               dashboard-insert-init-info
                               dashboard-insert-items)))

(use-package doom-modeline
    :ensure t
    :custom
    (doom-modeline-time-icon t)
    (doom-modeline-battery t)
    (doom-modeline-buffer-file-name-style 'truncate-upto-project)
    (doom-modeline-minor-modes nil)
    (doom-modeline-irc nil)
    (doom-modeline-mu4e nil)
    (doom-modeline-gnus nil)
    (doom-modeline-github nil)
    (doom-modeline-persp-name nil)
    :custom-face
    (mode-line ((t (:height 0.9))))
    (mode-line-inactive ((t (:height 0.9))))
    :hook (after-init . doom-modeline-mode))

(provide 'init-ui)

;;; init-ui.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; END: