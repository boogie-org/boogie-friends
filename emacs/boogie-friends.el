;;; boogie-friends.el --- Support for the Boogie-related languages in Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2015 Clément Pit--Claudel
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

;; This package is a collection of tools for writing verified programs in
;; languages of the Boogie family. Dafny and Boogie are the two currently
;; supported languages. Features include:
;;
;; * Syntax highlighting
;; * Real-time compilation (using flycheck)
;; * Completion (using company)
;; * Code folding (using hideshow)
;; * Prettification (using prettify-symbols-mode)
;;
;; In addition, the Dafny mode offers:
;; * (A few) Snippets (using yasnippet)
;; * (Some) In-Emacs documentation (using shr)
;; * (Experimental) Navigation between Dafny and Boogie source files
;; * (Some support for) indentation
;; * (Some support for) jumping to a definition
;;
;; See https://github.com/boogie-org/boogie-friends/ for a full description. The
;; documentation that accompanies certain snippets in dafny-mode was not written
;; as part of this package; it is automatically generated from Dafny's quick
;; reference guide.

;;; Code:

;; This file is a collection of convenient definitions and utilies for the
;; various programming modes that this package offers.

(require 'flycheck)
(require 'company)
(require 'yasnippet)
(require 'hideshow)
(require 'paren)
(require 'compile)

(defconst boogie-friends-directory (file-name-directory load-file-name)
  "Base directory of this package.")

(defconst boogie-friends-symbols-alist '(("<=" . ?≤) (">=" . ?≥) ("!=" . ?≠) (":=" . ?≔)
                                         ("&&" . ?∧) ("||" . ?∨) ("=>" . ?⇒)
                                         ("<==>" . ?⟺) ("==>" . ?⟹) ("<==" . ?⟸)
                                         ("exists" . ?∃) ("::" . ?∙))
  "Symbols used in conjunction with `prettify-minor-mode'.")

(defconst boogie-friends-message-pattern
  '(message (1+ nonl) (? "\nExecution trace:\n"
                         (1+ "    " (1+ nonl) (? "\n"))))
  "See `boogie-friends-error-patterns'.")

(defconst boogie-friends-error-patterns
  `((error bol (file-name) "(" line "," column "):" (or " Error " " Error: ")
           ,boogie-friends-message-pattern)
    (warning bol (file-name) "(" line "," column "):" " Related location: "
             ,boogie-friends-message-pattern))
  "Error patterns for the Dafny and Boogie checkers.")

(defvar boogie-friends-hooks nil
  "Hooks for Boogie friends customizations.
Use this hook to alter settings common to Dafny and Boogie, such
as prettification.")

(defconst boogie-friends-font-lock-var "\\_<\\(\\sw\\(?:\\sw\\|\\s_\\)*\\)\\_>"
  "Regexp used to detect variable names")

(defconst boogie-friends-font-lock-type "\\_<\\(\\sw+\\(?:<\\sw\\(?:\\sw\\|\\s_\\|[< ,>]\\)*>\\)?\\)\\_>"
  "Regexp used to detect type names.
Allowing (\\sw\\(?:\\sw\\|\\s_\\)*\\) in the first part
of the type causes Emacs to not parse sufficiently
greedily (the opening bracket is matched by \\s_.")

(defconst boogie-friends-font-lock-var-w-type (concat boogie-friends-font-lock-var "\\(\\(?:\\s-*:\\s-*" boogie-friends-font-lock-type "\\)?\\)")
  "Regexp used to detect variable names followed by a type")

(defconst boogie-friends-font-lock-assignment-chain (concat "\\(?:\\_<var\\_>\\s-*\\)?" "\\(?:" boogie-friends-font-lock-var-w-type "\\)"
                                                            "\\(?:\\s-*,\\s-*" boogie-friends-font-lock-var-w-type "\\)*\\s-*:[=|]")
  "Regexp used to detect [x, y:int, t := 1, 2, 3]")

(defun boogie-friends-mark-font-lock-assignment-chain (limit)
  "Font lock matcher function for multi-assignments."
  (when (re-search-forward boogie-friends-font-lock-assignment-chain limit t)
    (goto-char (match-end 2))))

(defun boogie-friends-verify (&optional arg)
  "Manually check the current file for errors. With prefix ARG, run the alternative checker if it exists."
  (interactive "P")
  (let ((buf (current-buffer)))
    (save-some-buffers nil (lambda () (eq buf (current-buffer)))))
  (unless (buffer-modified-p)
    (let* ((checker      (intern (boogie-friends-mode-name)))
           (extra-args-v (boogie-friends-mode-var 'checker-extra-args))
           (extra-args   (and (consp arg) (boundp extra-args-v) (eval (symbol-value extra-args-v))))
           (command      (concat (flycheck-checker-shell-command checker) " "
                                 (mapconcat #'shell-quote-argument extra-args " ")))
           (buffer       (compilation-start command nil)))
      (with-current-buffer buffer
        (set (make-local-variable 'compilation-error-regexp-alist)
             (flycheck-checker-compilation-error-regexp-alist checker))))))

(defun boogie-friends-format-header (err)
  "Format the first line of a Flycheck error ERR."
  (car (split-string (flycheck-error-message err) "\n")))

(defun boogie-friends-display-first-lines (errs)
  "Display first line of each error in ERRS."
  (when (and errs (flycheck-may-use-echo-area-p))
    (display-message-or-buffer (mapconcat #'boogie-friends-format-header errs "\n"))))

(defun boogie-friends-clean-overlay (var &optional buffer)
  "Remove temporary overlay in VAR, in buffer BUFFER."
  (setq buffer (or buffer (current-buffer)))
  (when (and buffer (buffer-live-p buffer))
    (with-current-buffer buffer
      (when (symbol-value var)
        (delete-overlay (symbol-value var))))))

(defun boogie-friends-mode-name ()
  "Name of current mode, without the '-mode' suffix."
  (replace-regexp-in-string "-mode\\'" "" (symbol-name major-mode)))

(defun boogie-friends-mode-var (suffix)
  "Append SUFFIX to name of current mode, returning a symbol."
  (intern (concat (boogie-friends-mode-name) "-" (symbol-name suffix))))

(defmacro boogie-friends-with-click (event mode set-point &rest body)
  "Run BODY in the buffer pointed to by EVENT, if in mode MODE.
If SET-POINT, place the point where EVENT points to."
  (declare (indent defun)
           (debug (form form form &rest form)))
  `(progn
     (save-excursion
       (mouse-set-point ,event)
       (-when-let* ((window  (posn-window (event-start event)))
                    (buffer  (window-buffer window)))
         (with-selected-window window
           (with-current-buffer buffer
             (when (eq major-mode ,mode)
               ,@body)))))
     (when ,set-point
       (mouse-set-point ,event))))

(defun boogie-friends-backward-line ()
  "Jump one line backwards, and then skip over blank lines."
  (forward-line 0)
  (skip-chars-backward "\r\n\t "))

(defun boogie-friends-cycle-indentation (&optional rev)
  "Cycle between reasonable indentation values for current line.
If REV is non-nil, cycle in the reverse order."
  (interactive)
  (let ((cur  (current-indentation))
        (prev (save-excursion (boogie-friends-backward-line) (current-indentation))))
    (if rev
        (indent-line-to (if (= cur 0) (indent-next-tab-stop prev) (indent-next-tab-stop cur rev)))
      (indent-line-to (if (> cur prev) 0 (indent-next-tab-stop cur rev))))))

(defun boogie-friends-keywords (command &optional arg &rest ignored)
  "A boogie-mode backend for keywords."
  (interactive (list 'interactive))
  (pcase command
    (`interactive (company-begin-backend 'boogie-friends-keywords))
    (`prefix (company-grab-symbol))
    (`candidates (let ((completion-ignore-case t)
                       (plen (length arg)))
                   (cl-loop for candidate in (all-completions arg (symbol-value (boogie-friends-mode-var 'all-keywords)))
                            do (put-text-property 0 (length candidate) 'match plen candidate)
                            collect candidate)))
    (`match (get-text-property 0 'match arg))
    (`annotation (replace-regexp-in-string "\\`.*-\\([^\\-]+s\\)\\'" "<\\1>" (symbol-name (get-text-property 0 'source arg))))
    (`ignore-case t)
    (`require-match 'never)))

(defun boogie-friends-cleanup-snippet (snippet)
  "Cleanup SNIPPET, yielding a better-looking string."
  (cl-loop for new-snippet = (replace-regexp-in-string "${\\([0-9]+:\\)?\\([^{}]+\\)}" "\\2" snippet)
           while (not (equal new-snippet snippet)) do (setq snippet new-snippet)
           finally return (replace-regexp-in-string "\\(\n\\|\\s-\\)+" " " new-snippet)))

(defun boogie-friends-insert-snippet (candidate)
  "Delete CANDIDATE, and insert the corresponding snippet."
  (-when-let* ((found   (search-backward candidate))
               (start   (match-beginning 0))
               (end     (match-end 0)))
    (yas-expand-snippet (get-text-property 0 'snippet candidate) start end)))

(defun boogie-friends-candidates-snippet (prefix snippets)
  "Find candidates matching PREFIX among SNIPPETS.
Matching is fuzzy."
  (when snippets
    (let* ((chars (string-to-list prefix))
           (quot  (lambda (c) (regexp-quote (char-to-string c))))
           (regex (concat "\\`\\W*" (mapconcat quot chars "\\(\\|.+?\\_<\\$?\\)"))))
      (save-match-data
        (cl-loop for snippet in snippets
                 when (string-match regex snippet)
                 do (put-text-property 0 (length snippet) 'match (match-end 0) snippet)
                 and collect snippet)))))

(defun boogie-friends-doc-buffer (&optional arg)
  "Show documentation for snippet ARG, if the current mode provides it."
  (-when-let* ((doc-function (boogie-friends-mode-var 'snippets-doc-buffer))
               (doc-buffer   (funcall doc-function arg)))
    doc-buffer))

(defun boogie-friends-snippets (command &optional arg &rest ignored)
  "A boogie-mode backend for snippets."
  (interactive (list 'interactive))
  (pcase command
    (`interactive (company-begin-backend 'boogie-friends-snippets))
    (`prefix (company-grab-symbol))
    (`candidates (let ((snippetsf (boogie-friends-mode-var 'init-snippets)))
                   (when (functionp snippetsf)
                     (boogie-friends-candidates-snippet arg (funcall snippetsf)))))
    (`match (get-text-property 0 'match arg))
    (`ignore-case t)
    (`sorted t)
    (`annotation "<snip>")
    (`doc-buffer (boogie-friends-doc-buffer arg))
    (`post-completion (boogie-friends-insert-snippet arg))
    (`no-cache t)
    (`require-match 'never)))

(defconst boogie-friends-ordered-backends
  '(boogie-friends-keywords boogie-friends-snippets company-dabbrev-code company-gtags company-etags)
  "List of backends to use for completion, enumerated in completion order.")

(defun boogie-friends-backend-index (candidate)
  "Find rank of source of CANDIDATE in `boogie-friends-ordered-backends'."
  (-if-let* ((backend (get-text-property 0 'company-backend candidate)))
      (cl-loop for b in boogie-friends-ordered-backends until (eq b backend) sum 1)
    0)) ;; No backend property means index is 0 (company optimization)

(defun boogie-friends-candidate-index (candidate)
  "Find rank of CANDIDATE among other values from same source."
  (or (get-text-property 0 'index candidate) 0))

(defun boogie-friends-match-length (candidate)
  "Find length of matched section of CANDIDATE."
  (or (get-text-property 0 'match candidate) 0))

(defun boogie-friends-sort-generic (seq predicates-alist)
  "Sort SEQ lexicographically.
PREDICATES-ALIST is an ALIST of (EXCTRACTION . COMPARISON).
values are compared by running EXTRACTION first, and then
COMPARISON.  If two extractions compare equal, the next alist
entry is used for comparison."
  (let ((comp (lambda (x1 x2)
                (cl-loop for (extractionf . comparisonf) in predicates-alist
                         for p1 = (funcall extractionf x1)
                         for p2 = (funcall extractionf x2)
                         when (not (equal p1 p2)) return (funcall comparisonf p1 p2)))))
    (cl-sort seq comp)))

(defun boogie-friends-sort-completion-candidates (candidates)
  "Sort completion candidates CANDIDATES.
Some duplicates are removed; results are ordered by backends,
then original index, match length, and finally by textual value."
  (let* ((alph  (boogie-friends-sort-generic candidates '((identity . (lambda (x y) (not (string-lessp x y))))
                                                          (boogie-friends-backend-index . <))))
         (dedup (cl-loop with prev = nil for cur in alph
                         when (not (and prev (string-match-p (concat "\\`" (regexp-quote cur)) prev))) collect cur
                         do (setq prev cur))))
    (boogie-friends-sort-generic dedup '((boogie-friends-backend-index   . <)
                                         (boogie-friends-candidate-index . <)
                                         (boogie-friends-match-length    . >)
                                         (identity                       . string-lessp)))))

(defun boogie-friends-setup-prettify ()
  "Setup `prettify-symbols-mode' in the current buffer.
Loads symbols from `boogie-friends-symbols-alist'."
  (set (make-local-variable 'prettify-symbols-alist) boogie-friends-symbols-alist)
  (prettify-symbols-mode 1))

(defun boogie-friends-setup-flycheck ()
  "Setup `flycheck-mode' in the current buffer.
Uses `boogie-friends-mode-name' as the name of the checker."
  (flycheck-mode)
  (let ((executable (flycheck-checker-executable (intern (boogie-friends-mode-name)))))
    (unless (executable-find executable)
      (message "Could not start checker for %s: '%s' not found. Please fix `flycheck-%s-executable'."
               (capitalize (boogie-friends-mode-name)) executable (boogie-friends-mode-name)))))

(defun boogie-friends-mode-setup ()
  "Setup the current buffer for Boogie-related editing."
  (set (make-local-variable 'tab-width) 2)
  (set (make-local-variable 'font-lock-defaults) (list (boogie-friends-mode-var 'font-lock-keywords)))
  (set (make-local-variable 'flycheck-display-errors-function) #'boogie-friends-display-first-lines)
  (set (make-local-variable 'comment-start) "//")
  (set (make-local-variable 'comment-start-skip) "/[*/]\\s-+")
  (make-local-variable 'company-transformers)
  (add-to-list 'company-transformers #'boogie-friends-sort-completion-candidates)
  (add-to-list (make-local-variable 'company-backends) boogie-friends-ordered-backends)
  (boogie-friends-setup-prettify)
  (yas-minor-mode)
  (show-paren-mode)
  (hs-minor-mode)
  (boogie-friends-setup-flycheck)
  (run-hooks 'boogie-friends-hook))

(provide 'boogie-friends)
;;; boogie-friends.el ends here
