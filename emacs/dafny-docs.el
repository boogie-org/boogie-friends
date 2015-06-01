;;; -*- lexical-binding: t -*-

(require 'shr)
(require 'dash)
(require 'boogie-friends)

(declare-function dafny-mode "dafny-mode.el")

(defun dafny-docs-shr-code (dom)
  (let ((start (point)))
    (shr-generic dom)
    (add-face-text-property start (point) 'font-lock-type-face))) ;; :family "Monospace"

(defun dafny-docs-shr-h2 (dom)
  (shr-heading dom '(:height 2.5)))

(defun dafny-docs-fontify-listing (text)
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
  (save-excursion
    (goto-char (point-min))
    (let ((regexp (regexp-opt (mapcar #'car boogie-friends-symbols-alist))))
      (while (re-search-forward regexp nil t)
        (let ((repl (char-to-string (cdr (assoc (match-string-no-properties 0) boogie-friends-symbols-alist)))))
          (replace-match repl nil t))))))

(defun dafny-docs-go-to-anchor ()
  (-when-let* ((target (when shr-target-id
                         (save-excursion
                           (goto-char (point-min))
                           (next-single-property-change (point-min) 'shr-target-id)))))
    (goto-char target)))

(defun dafny-docs-insert-docs (document)
  (let ((shr-target-id nil)
        (shr-use-fonts nil)
        (inhibit-read-only t)
        (inhibit-modification-hooks t)
        (shr-external-rendering-functions '((h2 . dafny-docs-shr-h2)
                                            (code . dafny-docs-shr-code)
                                            (pre . dafny-docs-shr-pre))))
    (erase-buffer)
    (shr-insert-document document)
    (dafny-docs-go-to-anchor)
    (dafny-docs-manual-prettification)
    (goto-char (point-min))))

(defun dafny-docs-open ()
  (interactive)
  (-when-let* ((document (with-temp-buffer
                           (insert-file-contents "clean.html")
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
