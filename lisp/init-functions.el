;;; init-functions.el --- the entry of emacs config -*- lexical-binding: t -*-
;; Author: Kiyu
;; Github: https://github.com/Kiyuim
;;; Commentary:
;; (c) Kiyu, 2024-
;;; Code:

(defun open-init-file()
    (interactive)
    (find-file (expand-file-name "init.el" user-emacs-directory )))
(global-set-key (kbd "<f2>") 'open-init-file)

(defun eh-orderless-regexp (orig_func component)
    (let ((result (funcall orig_func component)))
        (pyim-cregexp-build result)))
(defun toggle-chinese-search ()
    (interactive)
    (if (not (advice-member-p #'eh-orderless-regexp 'orderless-regexp))
        (advice-add 'orderless-regexp :around #'eh-orderless-regexp)
        (advice-remove 'orderless-regexp #'eh-orderless-regexp)))
(defun disable-py-search (&optional args)
    (if (advice-member-p #'eh-orderless-regexp 'orderless-regexp)
        (advice-remove 'orderless-regexp #'eh-orderless-regexp)))
;; (advice-add 'exit-minibuffer :after #'disable-py-search)
(add-hook 'minibuffer-exit-hook 'disable-py-search)
(global-set-key (kbd "s-p") 'toggle-chinese-search)

(defun consult-directory-externally (file)
    "Open FILE externally using the default application of the system."
    (interactive "fOpen externally: ")
    (if (and (eq system-type 'windows-nt)
            (fboundp 'w32-shell-execute))
        (shell-command-to-string (encode-coding-string (replace-regexp-in-string "/" "\\\\"
                                (format "explorer.exe %s" (file-name-directory (expand-file-name file)))) 'gbk))
    (call-process (pcase system-type
        ('darwin "open")
        ('cygwin "cygstart")
        (_ "xdg-open"))
            nil 0 nil
            (file-name-directory (expand-file-name file)))))

(defun open-current-directory ()
    (interactive)
    (consult-directory-externally default-directory))

(defun kiyu/consult-line (consult-line-function &rest rest)
    "Advising function around `CONSULT-LINE-FUNCTION'.
When there's an active region, use that as the first parameter
for `CONSULT-LINE-FUNCTION'.  Otherwise, use the current word as
the first parameter.  This function handles the `REST' of the
parameters."
    (interactive)
    (if (use-region-p)
        (apply consult-line-function
                (buffer-substring (region-beginning) (region-end)) rest)
        (apply consult-line-function
            rest)))

(defun +vertico/embark-export-write ()
    "Export the current vertico results to a writable buffer if possible.
Supports exporting consult-grep to wgrep, file to wdeired, and consult-location to occur-edit"
    (interactive)
    (require 'embark)
    (require 'wgrep)
    (pcase-let ((`(,type . ,candidates)
            (run-hook-with-args-until-success 'embark-candidate-collectors)))
    (pcase type
        ('consult-grep (let ((embark-after-export-hook #'wgrep-change-to-wgrep-mode))
                (embark-export)))
        ('file (let ((embark-after-export-hook #'wdired-change-to-wdired-mode))
                (embark-export)))
        ('consult-location (let ((embark-after-export-hook #'occur-edit-mode))
                (embark-export)))
        (x (user-error "embark category %S doesn't support writable export" x)))))

(defun +vertico/embark-preview ()
    "Previews candidate in vertico buffer, unless it's a consult command"
    (interactive)
    (unless (bound-and-true-p consult--preview-function)
        (save-selected-window
            (let ((embark-quit-after-action nil))
            (embark-dwim)))))

(provide 'init-functions)

;;; init-functions.el ends here
;;; Local Variables:
;; coding: utf-8
;; byte-compile-warnings: (not unresolved obsolete)
;; End: