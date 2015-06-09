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

(defgroup boogie-friends nil
  "IDE extensions for the programming languages of the Boogie family."
  :group 'languages)

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

(defcustom boogie-friends-profiler-timeout 30
  "Timeout used when profiling.
This value is read by `boogie-friends-profile', which see.  It
must an whole number of seconds.")

(defvar boogie-friends--prover-additional-args nil
  "Storage for extra prover arguments.
Only for temporary assignment of internal values")

(defvar boogie-friends-last-trace nil
  "Cache of the last trace information obtained for this buffer.")

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
greedily (the opening bracket is matched by \\s_).")

(defconst boogie-friends-font-lock-array-sub "\\(?:\\[[^\\[\\]]+\\]\\)"
  "Regexp used to detect array subscriptions")

(defconst boogie-friends-font-lock-var-w-type (concat boogie-friends-font-lock-var boogie-friends-font-lock-array-sub "?\\(\\(?:\\s-*:\\s-*" boogie-friends-font-lock-type "\\)?\\)")
  "Regexp used to detect variable names optionally followed by a type")

(defconst boogie-friends-font-lock-assignment-chain (concat "\\(?:\\_<var\\_>\\s-*\\)?" "\\(?:" boogie-friends-font-lock-var-w-type "\\)"
                                                            "\\(?:\\s-*,\\s-*" boogie-friends-font-lock-var-w-type "\\)*\\s-*:[=|]")
  "Regexp used to detect [x, y:int, t := 1, 2, 3]")

(defun boogie-friends-mark-font-lock-assignment-chain (limit)
  "Font lock matcher function for multi-assignments."
  (when (re-search-forward boogie-friends-font-lock-assignment-chain limit t)
    (goto-char (match-end 2))))

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

(defun boogie-friends-mode-val (suffix)
  "Retrieves the value of (boogie-friends-mode-var SUFFIX)."
  (symbol-value (boogie-friends-mode-var suffix)))

(defun boogie-friends-compute-prover-args ()
  "Compute the set of arguments to pass to the prover."
  (append (boogie-friends-mode-val 'prover-args)
          (boogie-friends-mode-val 'prover-custom-args)
          (boogie-friends-mode-val 'prover-local-args)
          boogie-friends--prover-additional-args))

(defun boogie-friends--flycheck-compile-wrapper (checker)
  "Like `flycheck-compile', but return the compile buffer."
  (unless (flycheck-may-use-checker checker)
    (user-error "Cannot use syntax checker %S in this buffer" checker))
  (let* ((command (flycheck-checker-shell-command checker))
         (buffer (compilation-start command nil #'flycheck-compile-name)))
    (with-current-buffer buffer
      (set (make-local-variable 'compilation-error-regexp-alist)
           (flycheck-checker-compilation-error-regexp-alist checker))
      buffer)))

(defun boogie-friends--compile (additional-arguments use-alternate)
  "Start compiling the current file.
Add ADDITIONAL-ARGUMENTS to usual command line, placing them
after alternate prover args if USE-ALTERNATE is non-nil.  This function
is useful to implement user-initiated verification, as well as
tracing.  Returns the compile buffer."
  (let ((buf (current-buffer)))
    (save-some-buffers nil (lambda () (eq buf (current-buffer)))))
  (unless (buffer-modified-p)
    (let* ((custom-args (and use-alternate (boogie-friends-mode-val 'prover-alternate-args)))
           (boogie-friends--prover-additional-args (append custom-args additional-arguments)))
      (boogie-friends--flycheck-compile-wrapper (intern (boogie-friends-mode-name))))))

(defun boogie-friends-verify (&optional arg)
  "Manually check the current file for errors.
With prefix ARG, run the checker with custom args."
  (interactive "P")
  (boogie-friends--compile nil (consp arg)))

(defun boogie-friends-get-timeout-arg ()
  (list (format "/timeLimit:%d" boogie-friends-profiler-timeout)))

(defun boogie-friends-get-trace-args ()
  "Build arguments to pass to Dafny or Boogie to produce a trace.
Used by `boogie-friends-trace', which see. If NO-TIMEOUT is
non-nil, each method is restricted to
`boogie-friends-profiler-timeout' seconds."
  (append (list "/trace")
          (boogie-friends-get-timeout-arg)))

(defun boogie-friends-get-profile-args (proc)
  "Build arguments to pass to Dafny or Boogie to produce a profile.
Used by `boogie-friends-trace', which see. If NO-TIMEOUT is
non-nil, each method is restricted to
`boogie-friends-profiler-timeout' seconds."
  (append (list "/z3opt:TRACE=true")
          (when (stringp proc) (list (format "/proc:%s" proc)))
          (boogie-friends-get-timeout-arg)))

(defcustom boogie-friends-profile-analyzer-executable "Z3AxiomProfiler.exe"
  "The path to a program able to read Z3 traces.")

(defun boogie-friends-profiler-callback (log-path) ;; FIXME prefix arg to select subreport?
  (-if-let (exec (executable-find boogie-friends-profile-analyzer-executable))
      (start-process "*DafnyProfilerCallback" " *DafnyProfilerCallback*" exec (list log-path))
    (message "Executable not found: %s" boogie-friends-profile-analyzer-executable)))

(defun boogie-friends-make-trace-callback (source-buffer compilation-buffer)
  (lambda (callback-buffer _status)
    (when (and (eq callback-buffer compilation-buffer)
               (buffer-live-p callback-buffer))
      (-when-let (trace (with-current-buffer callback-buffer (boogie-friends-parse-trace)))
        (with-current-buffer source-buffer
          (set (make-local-variable 'boogie-friends-last-trace) trace)
          (message "Trace results collected!"))))))

(defun boogie-friends-trace (&optional use-alternate)
  "Manually check the current file for errors, producing a trace.
With prefix USE-ALTERNATE, run the checker with alternate args."
  (interactive "P")
  (-when-let* ((trace-args   (boogie-friends-get-trace-args))
               (compilation-buffer (boogie-friends--compile trace-args  (consp use-alternate)))
               (trace-parser (boogie-friends-make-trace-callback (current-buffer) compilation-buffer)))
    (with-current-buffer compilation-buffer
      (add-hook 'compilation-finish-functions trace-parser nil t))))

(defconst boogie-friends-profiler-whole-file-choice "[[Whole file]]")

(defun boogie-friends-profiler-interact-prepare-completions ()
  (let* ((candidates (mapcar (lambda (entry) (format "[%.2fs] %s" (cdr entry) (car entry))) boogie-friends-last-trace))
         (newcdr     (cons boogie-friends-profiler-whole-file-choice (cdr-safe candidates))))
    (if candidates
        (cons (car candidates) newcdr)
      newcdr)))

(defun boogie-friends-profiler-interact ()
  (let* ((msg        (if boogie-friends-last-trace "Function to complete (TAB for completion): "
                       "Function name (use \\[boogie-friends-trace] first to enable completion): "))
         (prompt     (substitute-command-keys msg))
         (collection (boogie-friends-profiler-interact-prepare-completions))
         (selected   (completing-read prompt collection nil nil nil nil (car-safe collection)))
         (cleaned-up (if (member selected collection) (replace-regexp-in-string "^\\[[^ ]*\\] " "" selected) selected))
         (proc       (unless (member cleaned-up `(nil "" ,boogie-friends-profiler-whole-file-choice)) cleaned-up)))
  (list proc (consp current-prefix-arg))))

(defun boogie-friends-make-profiler-callback (source-buffer compilation-buffer)
  (with-current-buffer source-buffer
    (-when-let* ((fname buffer-file-name))
      (lambda (callback-buffer status)
        (when (and (eq callback-buffer compilation-buffer)
                   (or (string-match-p "finished" status) ;; Timeouts may cause abnormal exits
                       (string-match-p "exited abnormally" status)))
          (boogie-friends-profiler-callback (expand-file-name "z3.log" (file-name-directory fname))))))))

(defun boogie-friends-profile (func &optional use-alternate)
  "Profile a given function FUNC, or the whole file is FUNC is nil.
After invoking the relevant profiling command, call a
mode-specific function to handle the profile trace. When
USE-ALTERNATE is non-nil, use alternate prover args. For each
method profiling stops after `boogie-friends-profiler-timeout'
seconds."
  (interactive (boogie-friends-profiler-interact))
  (-when-let* ((profiler-args (boogie-friends-get-profile-args func))
               (compilation-buffer (boogie-friends--compile profiler-args use-alternate))
               (profiler-post-action (boogie-friends-make-profiler-callback (current-buffer) compilation-buffer)))
    (with-current-buffer compilation-buffer
      (add-hook 'compilation-finish-functions profiler-post-action nil t))))

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
  (/= 0 (skip-chars-backward "\r\n\t ")))

(defun boogie-friends-cycle-indentation (&optional rev)
  "Cycle between reasonable indentation values for current line.
If REV is non-nil, cycle in the opposite order."
  (interactive)
  (let ((cur  (current-indentation))
        (prev (save-excursion (boogie-friends-backward-line) (current-indentation))))
    (if (not rev)
        (indent-line-to (if (= cur 0) (indent-next-tab-stop prev) (indent-next-tab-stop cur (not rev))))
      (indent-line-to (if (> cur prev) 0 (indent-next-tab-stop cur (not rev)))))))

(defun boogie-friends-self-insert-and-indent (arg)
  (interactive "p")
  (self-insert-command arg)
  (when (functionp indent-line-function)
    (funcall indent-line-function)))

(defconst boogie-friends-trace-entry-regexp
  "^Verifying\\s-*\\([^ ]+\\)\\s-*...\\s-*\\[\\([^ ]+\\)\\s-+s,.*\\]"
  "Regexp used to locate useful timings from a Boogie trace.")

(defun boogie-friends-parse-trace-entry ()
  "Parse one entry from a Boogie trace."
  (cons
   (match-string-no-properties 1)
   (string-to-number (match-string-no-properties 2))))

(defun boogie-friends-parse-trace ()
  "Parse a Boogie trace.
This function should be called from a Boogie compilation
buffer.  The return value is a list of cons of the
form (FUNCTION-NAME . TIME)"
  (save-excursion
    (goto-char (point-min))
    (cl-sort (cl-loop while (re-search-forward boogie-friends-trace-entry-regexp nil t)
                   for entry = (ignore-errors (boogie-friends-parse-trace-entry))
                   when entry collect entry)
             #'> :key #'cdr)))

(defun boogie-friends-translation-buffer-file-names ()
  "Computes a buffer name for translating to lower level source."
  (-when-let* ((fname (buffer-file-name))
               (ext   (boogie-friends-mode-val 'translation-extension)))
    (cons (concat (buffer-name) ext) (concat fname ext))))

(defun boogie-friends-translation-filter (proc string)
  "Filter function for the source translation process PROC.
Inserts STRING at end of buffer.  Does not do automatic
scrolling."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((inhibit-read-only t)
            (prev-location (point)))
        (save-excursion
          (goto-char (process-mark proc))
          (insert string)
          (set-marker (process-mark proc) (point)))
        (goto-char (min (point-max) prev-location))))))

(defun boogie-friends-translation-sentinel (proc _sig)
  "Sentinel function for the source translation process PROC.
Saves the buffer upon completion of the process, and prevents the insertion
of a termination message after the conversion completes."
  (when (buffer-live-p (process-buffer proc))
    (let ((src-callback (boogie-friends-mode-var 'translation-sentinel-src-callback))
          (dst-callback (boogie-friends-mode-var 'translation-sentinel-dst-callback)))
      (with-current-buffer (process-buffer proc)
        (when (functionp dst-callback) (funcall dst-callback))
        (when buffer-file-name (save-buffer)))
      (when (functionp src-callback) (funcall src-callback)))))

(defun boogie-friends-translation-sentinel-cleanup (regexp)
  "Remove line matching REGEXP at end of buffer."
  (save-excursion
    (goto-char (point-max))
    (when (re-search-backward regexp (save-excursion (forward-line -5) (point)) t)
      (let ((inhibit-read-only t)) (replace-match "" t t)))))

(defun boogie-friends-get-buffer-unless-rw (buf-name)
  "Convenience wrapper around `get-buffer-create'.
Throws if a buffer of name BUF-NAME already exists and is not read-only;
otherwise, returns that buffer, or a newly created one of that
name is none is found."
  (-when-let* ((buffer (get-buffer buf-name)))
    (when (not (with-current-buffer buffer buffer-read-only))
      (error "Buffer %s already exists and is not read-only.  Cowardly refusing to overwrite it" buf-name)))
  (get-buffer-create buf-name))

(defun boogie-friends-translate-source ()
  "Translate to lower level language, save the resulting file, and display it."
  (interactive)
  (let ((buf (current-buffer)))
    (save-some-buffers nil (lambda () (eq buf (current-buffer)))))
  (-when-let* ((dst-mode       (boogie-friends-mode-val 'translation-target-mode))
               (src-name       buffer-file-name)
               ((dst-buf-name . dst-file-name) (boogie-friends-translation-buffer-file-names))
               (buffer         (boogie-friends-get-buffer-unless-rw dst-buf-name))
               (proc-name      (boogie-friends-mode-val 'translation-proc-name))
               (translate-args (boogie-friends-mode-val 'translation-prover-args))
               (cmd            (cons (flycheck-checker-executable (intern (boogie-friends-mode-name)))
                                     (append (boogie-friends-compute-prover-args)
                                             translate-args (list src-name)))))
    (-when-let* ((proc (get-buffer-process buffer)))
      (ignore-errors (kill-process proc) (accept-process-output)))
    (boogie-friends-prepare-translation-buffer buffer dst-mode cmd dst-file-name)
    (let ((proc (apply #'start-process proc-name buffer cmd)))
      (set-process-filter proc #'boogie-friends-translation-filter)
      (set-process-sentinel proc #'boogie-friends-translation-sentinel))))

(defun boogie-friends-prepare-translation-buffer (buffer mode command-line file-name)
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (erase-buffer)
      (funcall mode)
      (insert (mapconcat #'identity command-line " "))
      (comment-region (point-min) (point-max))
      (insert "\n"))
    (setq buffer-file-name file-name)
    (read-only-mode))
  (display-buffer buffer))

(defun boogie-friends-make-keymap ()
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "}") #'boogie-friends-self-insert-and-indent)
    (define-key map (kbd "C-c C-c") #'boogie-friends-verify)
    (define-key map (kbd "C-c C-t") #'boogie-friends-trace)
    (define-key map (kbd "C-c C-p") #'boogie-friends-profile)
    (define-key map (kbd "<backtab>") #'boogie-friends-cycle-indentation)
    map))

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

(defun boogie-friends-mode-setup (&optional minimal)
  "Setup the current buffer for Boogie-related editing."
  (unless minimal
    (set (make-local-variable 'tab-width) 2)
    (set (make-local-variable 'font-lock-defaults) (list (boogie-friends-mode-var 'font-lock-keywords)))
    (set (make-local-variable 'flycheck-display-errors-function) #'boogie-friends-display-first-lines)
    (set (make-local-variable 'comment-start) "//")
    (set (make-local-variable 'comment-start-skip) "/[*/]\\s-+")
    (make-local-variable 'company-transformers)
    (add-to-list 'company-transformers #'boogie-friends-sort-completion-candidates)
    (add-to-list (make-local-variable 'company-backends) boogie-friends-ordered-backends)
    (boogie-friends-setup-prettify)
    (yas-minor-mode))
  (company-mode)
  (hs-minor-mode)
  (show-paren-mode)
  (boogie-friends-setup-flycheck)
  (run-hooks 'boogie-friends-hook))

(provide 'boogie-friends)
;;; boogie-friends.el ends here
