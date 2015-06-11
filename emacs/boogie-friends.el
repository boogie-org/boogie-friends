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
(require 'ido)

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

(defun boogie-friends-save-or-error ()
  (let ((buf (current-buffer)))
    (save-some-buffers nil (lambda () (eq buf (current-buffer)))))
  (unless (and buffer-file-name (not (buffer-modified-p)))
    (error "Cannot run this command on a dirty source file")))

(defun boogie-friends-compilation-buffer-namer (infix)
  (let ((name (format "*%s-%s-%s*" (boogie-friends-mode-name) infix buffer-file-name)))
  (lambda (_mode) name)))

(defun boogie-friends--compile (additional-arguments use-alternate name)
  "Start compiling the current file.
Add ADDITIONAL-ARGUMENTS to usual command line, placing them
after alternate prover args if USE-ALTERNATE is non-nil.  This function
is useful to implement user-initiated verification, as well as
tracing.  Returns the compile buffer."
  (boogie-friends-save-or-error)
  (let ((checker (intern (boogie-friends-mode-name))))
    (unless (flycheck-may-use-checker checker)
      (user-error "Prover %s is improperly configured" checker))
    (let* ((custom-args (and use-alternate (boogie-friends-mode-val 'prover-alternate-args)))
           (boogie-friends--prover-additional-args (append custom-args additional-arguments))
           (command (flycheck-checker-shell-command checker))
           (buffer (compilation-start command nil (boogie-friends-compilation-buffer-namer name))))
      (with-current-buffer buffer
        (set (make-local-variable 'compilation-error-regexp-alist)
             (flycheck-checker-compilation-error-regexp-alist checker)))
      buffer)))

(defun boogie-friends-verify (&optional arg)
  "Manually check the current file for errors.
With prefix ARG, run the checker with custom args."
  (interactive "P")
  (boogie-friends--compile nil (consp arg) "verification"))

(defun boogie-friends-get-timeout-arg ()
  (list (format "/timeLimit:%d" boogie-friends-profiler-timeout)))

(defun boogie-friends-get-trace-args ()
  "Build arguments to pass to Dafny or Boogie to produce a trace.
Used by `boogie-friends-trace', which see. If NO-TIMEOUT is
non-nil, each method is restricted to
`boogie-friends-profiler-timeout' seconds."
  (append (list "/trace")
          (boogie-friends-get-timeout-arg)))

(defun boogie-friends-make-trace-callback (source-buffer compilation-buffer)
  (lambda (callback-buffer _status)
    (when (and (eq callback-buffer compilation-buffer)
               (buffer-live-p callback-buffer))
      (-when-let (trace (with-current-buffer callback-buffer (boogie-friends-parse-trace)))
        (with-current-buffer source-buffer
          (set (make-local-variable 'boogie-friends-last-trace) trace)
          (message (substitute-command-keys "Trace results collected! Use \\[boogie-friends-profile] to start profiling")))))))

(defun boogie-friends-trace (&optional use-alternate)
  "Manually check the current file for errors, producing a trace.
With prefix USE-ALTERNATE, run the checker with alternate args."
  (interactive "P")
  (boogie-friends-save-or-error)
  (-when-let* ((trace-args   (boogie-friends-get-trace-args))
               (compilation-buffer (boogie-friends--compile trace-args (consp use-alternate) "trace"))
               (trace-parser (boogie-friends-make-trace-callback (current-buffer) compilation-buffer)))
    (with-current-buffer compilation-buffer
      (add-hook 'compilation-finish-functions trace-parser nil t))))

(defun boogie-friends-ensure-buffer-ro (buffer-fname)
  "Ensures that any buffer visiting VUFFER-FNAME is readonly.
Throws if a counter-example is found."
  (-when-let* ((visiting-buffer (find-buffer-visiting buffer-fname)))
    (with-current-buffer visiting-buffer
      (unless buffer-read-only
        (error "Buffer %s is modified and already visiting %s; cowardly refusing to overwrite" (buffer-name) buffer-fname))))
  t)

(defun boogie-friends-translate-callback (translated-fname source-buffer compilation-buffer continuation callback-buffer status)
  (when (and (eq callback-buffer compilation-buffer)
             (string-match-p "finished" status))
    (let ((buf (find-buffer-visiting translated-fname))
          (local-vars (with-current-buffer source-buffer file-local-variables-alist)))
      (if buf (with-current-buffer buf (revert-buffer t t))
        (setq buf (find-file-noselect translated-fname)))
      (when buf
        (kill-buffer compilation-buffer)
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            ;; Port local variables to translated buffer
            (when local-vars
              (cl-loop for (var . val) in local-vars
                       unless (eq var 'mode)
                       do (add-file-local-variable var val))
              (save-buffer))
            (goto-char (point-min)))
          (read-only-mode))
        (-if-let* ((source-wind (and (buffer-live-p source-buffer) (get-buffer-window source-buffer))))
            (with-selected-window source-wind (display-buffer buf))
          (display-buffer buf))
        (when (functionp continuation)
          (funcall continuation source-buffer buf))))))

(defun boogie-friends-make-translate-callback (translated-fname source-buffer compilation-buffer continuation)
  (lambda (callback-buffer status)
    (boogie-friends-translate-callback translated-fname source-buffer compilation-buffer
                                       continuation callback-buffer status)))

(defun boogie-friends-translated-fname ()
  (-when-let* ((translated-ext (boogie-friends-mode-val 'translation-extension)))
    (concat buffer-file-name translated-ext)))

(defun boogie-friends-translate (&optional use-alternate callback)
  "Translate to lower level language, save the resulting file, and display it."
  (interactive "P")
  (boogie-friends-save-or-error)
  (-when-let* ((translated-fname (boogie-friends-translated-fname))
               (refuse-overwriting (boogie-friends-ensure-buffer-ro translated-fname))
               (translate-args-f (boogie-friends-mode-var 'translation-prover-args-fn))
               (translated-args (and (functionp translate-args-f) (funcall translate-args-f translated-fname)))
               (compilation-buffer (boogie-friends--compile translated-args use-alternate "translate"))
               (translate-callback (boogie-friends-make-translate-callback translated-fname (current-buffer) compilation-buffer callback)))
    (with-current-buffer compilation-buffer
      (add-hook 'compilation-finish-functions translate-callback nil t))))

(defconst boogie-friends-profiler-whole-file-choice "Whole file")

(defun boogie-friends-get-profile-args (log-path proc)
  "Build arguments to pass to Dafny or Boogie to produce a profile.
Used by `boogie-friends-trace', which see. If NO-TIMEOUT is
non-nil, each method is restricted to
`boogie-friends-profiler-timeout' seconds."
  (append (list "/z3opt:TRACE=true")
          (when log-path (format "/z3opt:TRACE_FILE_NAME=\"%s\"" log-path)) ;; FIXME
          (when (stringp proc) (list (format "/proc:%s" proc)))
          (boogie-friends-get-timeout-arg)))

(defcustom boogie-friends-profile-analyzer-executable "Z3AxiomProfiler.exe"
  "The path to a program able to read Z3 traces.")

(defun boogie-friends-profiler-callback (source-path log-path) ;; FIXME prefix arg to select subreport?
  (-if-let* ((exec (executable-find boogie-friends-profile-analyzer-executable))
             (prog (list exec source-path (concat "/l:" log-path)))
             (cmd  (mapconcat #'shell-quote-argument prog " ")))
      (progn
        (message "Launching profiler; use [%s] to launch again manually" cmd)
        (apply #'start-process "*DafnyProfilerCallback" " *DafnyProfilerCallback*" prog))
    (message "Executable not found: %s" boogie-friends-profile-analyzer-executable)))

(defun boogie-friends-make-profiler-callback (source-path log-path compilation-buffer)
  (lambda (callback-buffer status)
    (when (and (eq callback-buffer compilation-buffer)
               (or (string-match-p "finished" status) ;; Timeouts may cause abnormal exits
                   (string-match-p "exited abnormally" status)))
      (boogie-friends-profiler-callback source-path log-path))))

(defun boogie-friends-profiler-interact-prepare-completions ()
  (let* ((candidates (mapcar (lambda (entry) (format "%s (%.2fs)" (car entry) (cdr entry))) boogie-friends-last-trace))
         (newcdr     (cons boogie-friends-profiler-whole-file-choice (cdr-safe candidates))))
    (if candidates
        (cons (car candidates) newcdr)
      newcdr)))

(defun boogie-friends-profiler-interact ()
  (boogie-friends-save-or-error)
  (let* ((msg        (if boogie-friends-last-trace "Function to profile: "
                       "Function name (use \\[boogie-friends-trace] first to enable completion): "))
         (prompt     (substitute-command-keys msg))
         (collection (boogie-friends-profiler-interact-prepare-completions))
         (selected   (ido-completing-read prompt collection nil nil nil nil (car-safe collection)))
         (cleaned-up (if (member selected collection) (replace-regexp-in-string " ([^ ]*)$" "" selected) selected))
         (proc       (unless (member cleaned-up `(nil "" ,boogie-friends-profiler-whole-file-choice)) cleaned-up)))
  (list proc (consp current-prefix-arg))))

(defun boogie-friends-profiler-prepare (func use-alternate)
  (-when-let* ((profiler-prepare-fn (boogie-friends-mode-var 'profiler-prepare-fn)))
    (funcall profiler-prepare-fn use-alternate
             (lambda (source-path) (boogie-friends-profile-internal func use-alternate source-path)))))

(defun boogie-friends-profile-internal (func use-alternate source-path)
  (-when-let* ((log-path (expand-file-name "z3.log" (file-name-directory source-path))) ;; FIXME
               (profiler-args (boogie-friends-get-profile-args nil func)) ;; FIXME nil instead of log-path
               (compilation-buffer (boogie-friends--compile profiler-args use-alternate "profile"))
               (profiler-post-action (boogie-friends-make-profiler-callback source-path log-path compilation-buffer)))
    (with-current-buffer compilation-buffer
      (add-hook 'compilation-finish-functions profiler-post-action nil t))))

(defun boogie-friends-profile (func &optional use-alternate)
  "Profile a given function FUNC, or the whole file is FUNC is nil.
After invoking the relevant profiling command, call a
mode-specific function to handle the profile trace. When
USE-ALTERNATE is non-nil, use alternate prover args. For each
method profiling stops after `boogie-friends-profiler-timeout'
seconds."
  (interactive (boogie-friends-profiler-interact))
  (boogie-friends-save-or-error)
  (boogie-friends-profiler-prepare func use-alternate))

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

(defun boogie-friends-make-keymap (&optional include-profiling)
  "Constructs a keemap suitable for boogie-related languages.
If INCLUDE-PROFILING is non-nil, add keybindings for tracing and profiling."
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "}") #'boogie-friends-self-insert-and-indent)
    (define-key map (kbd "C-c C-p") #'boogie-friends-profile)
    (when include-profiling
      (define-key map (kbd "C-c C-c") #'boogie-friends-verify)
      (define-key map (kbd "C-c C-t") #'boogie-friends-trace))
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
