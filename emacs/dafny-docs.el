;;; dafny-docs.el --- Simple in-Emacs documentation for Dafny -*- lexical-binding: t -*-

;; Copyright (C) 2015, 2016 Clément Pit--Claudel
;; Author: Clément Pit--Claudel <clement.pitclaudel@live.com>
;; URL: https://github.com/boogie-org/boogie-friends/

;; Keywords: convenience, languages

;; This file is not part of GNU Emacs.

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; See boogie-friends.el

;;; Code:

;; This file contains support for parsing and rendering Dafny's Quick Reference.

(require 'shr)
(require 'dash)
(require 'boogie-friends)

(declare-function dafny-mode "dafny-mode.el")

(defun dafny-docs-shr-code (dom)
  "Render a code block DOM as part of shr's layout pass."
  (let ((start (point)))
    (shr-generic dom)
    (add-face-text-property start (point) 'font-lock-type-face))) ;; :family "Monospace"

(defun dafny-docs-shr-h2 (dom)
  "Render a title DOM as part of shr's layout pass."
  (shr-heading dom '(:height 2.5)))

(defun dafny-docs-fontify-listing (text)
  "Highlight TEXT as a Dafny source code listing."
  (with-temp-buffer
    (insert text)
    (delay-mode-hooks (dafny-mode))
    (set (make-local-variable 'prettify-symbols-alist) boogie-friends-symbols-alist)
    (prettify-symbols-mode)
    (font-lock-default-function #'dafny-mode)
    (font-lock-default-fontify-region (point-min) (point-max) nil)
    (goto-char (point-min))
    (while (search-forward " " nil t)
      (replace-match " " nil t))
    (indent-rigidly-right-to-tab-stop (point-min) (point-max))
    (buffer-string)))

(defun dafny-docs-shr-pre (dom)
  "Render a code block DOM as part of shr's layout pass."
  (let ((start (point))
        (shr-folding-mode 'none)
        (shr-current-font 'default))
    (shr-ensure-newline)
    (shr-generic dom)
    (let* ((region    (buffer-substring start (point)))
           (fontified (dafny-docs-fontify-listing region)))
      (delete-region start (point))
      (insert fontified))))

(defun dafny-docs-manual-prettification ()
  "Transform textual symbols into their math equivalents.
Symbols are read from `boogie-friends-symbols-alist'.  This is
useful to avoid conflicts between the results of
`dafny-docs-fontify-listing' and the operation of
`prettify-symbols-mode'."
  (save-excursion
    (goto-char (point-min))
    (let ((regexp (regexp-opt (mapcar #'car boogie-friends-symbols-alist))))
      (while (re-search-forward regexp nil t)
        (let ((repl (char-to-string (cdr (assoc (match-string-no-properties 0) boogie-friends-symbols-alist)))))
          (replace-match repl nil t))))))

(defun dafny-docs-insert-docs (document)
  "Insert Dafny's quick reference DOCUMENT in the current buffer."
  (let ((shr-use-fonts nil)
        (inhibit-read-only t)
        (inhibit-modification-hooks t)
        (shr-external-rendering-functions '((h2 . dafny-docs-shr-h2)
                                            (code . dafny-docs-shr-code)
                                            (pre . dafny-docs-shr-pre))))
    (erase-buffer)
    (shr-insert-document document)
    (dafny-docs-manual-prettification)
    (goto-char (point-min))))

(defun dafny-docs-open ()
  "Load and display Dafny's quick reference guide."
  (interactive)
  (-when-let* ((document (with-temp-buffer
                           (insert-file-contents (expand-file-name "etc/dafny-docs.html.gz" boogie-friends-directory))
                           (libxml-parse-html-region (point-min) (point-max)))))
    (with-current-buffer (get-buffer-create "*dafny-docs*")
      (help-mode)
      (read-only-mode 1)
      (buffer-disable-undo)
      (set (make-local-variable 'show-trailing-whitespace) nil)
      (when (display-buffer (current-buffer))
        (dafny-docs-insert-docs document))
      (current-buffer))))

(provide 'dafny-docs)
;;; dafny-docs.el ends here
