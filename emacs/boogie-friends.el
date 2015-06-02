 ;;; -*- lexical-binding: t -*-

(require 'flycheck)
(require 'company)
(require 'yasnippet)
(require 'hideshow)
(require 'paren)

(defconst boogie-friends-directory (file-name-directory load-file-name)
  "Base directory of this package")

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

(defun boogie-friends-verify ()
  (interactive)
  "Manually check the current file for errors."
  (let ((buf (current-buffer)))
    (save-some-buffers nil (lambda () (eq buf (current-buffer)))))
  (unless (buffer-modified-p)
    (flycheck-compile (intern (boogie-friends-mode-name)))))

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

(defun boogie-friends-keywords (command &optional arg &rest ignored)
  "A company-mode backend for keywords."
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
  "A company-mode backend for snippets."
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
