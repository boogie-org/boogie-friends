(load (expand-file-name "package-name.el" (file-name-directory load-file-name)))

(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)
(package-refresh-contents)
(package-install-file (expand-file-name (concat my-package-version ".tar") "build"))

(setq garbage-collection-messages t)
