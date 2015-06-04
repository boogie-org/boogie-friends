(load (expand-file-name "package-name.el" (file-name-directory load-file-name)))

(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)
(package-refresh-contents)

(setq garbage-collection-messages t)
