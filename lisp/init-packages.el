;;; init-package.el -*- lexical-binding: t no-byte-compile: t -*-
;; Author: Kiyu
;; Github: https://github.com/Kiyuim
;;; Commentary:
;; Runs before package and UI initializetion happens.
;; (c) Kiyu, 2024-
;;; Code:

(require 'package)
    (setq package-archives
    	'(("melpa"  . "https://melpa.org/packages/")
        ("gnu"    . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(package-initialize)

;; Preventing repeated calls package-refresh-contents
(when (not package-archive-contents)
    (package-refresh-contents))

;; Setup `use-package'
(unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

(require 'use-package-ensure)
    (setq use-package-always-ensure t)
    (setq use-package-always-defer t)

;; Setup `quelpa'
(use-package quelpa)

(unless (package-installed-p 'quelpa-use-package)
    (quelpa
        '(quelpa-use-package
            :fetcher git
            :url "https://github.com/quelpa/quelpa-use-package.git")))

(use-package quelpa-use-package
    :init
    (setq quelpa-use-package-inhibit-loading-quelpa t)
        :demand t)
        
(require 'cl-lib)
(require 'use-package-core)

(provide 'init-packages)

;;; init-packages.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; END: