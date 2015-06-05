;;; dafny-mode.el --- Support for the Dafny programming language -*- lexical-binding: t -*-

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

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

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

;; This file contains the implementation of the Dafny part of the boogie-friends
;; package

(require 'boogie-friends)
(require 'boogie-mode)
(require 'dafny-docs)

(defconst dafny-defuns '("class" "codatatype" "colemma" "constructor" "copredicate" "datatype" "function" "inductive"
                         "iterator" "lemma" "method" "newtype" "predicate" "trait" "type"))

(defconst dafny-specifiers '("decreases" "ensures" "free" "invariant" "modifies" "reads" "requires"))

(defconst dafny-modifiers '("abstract" "ghost" "protected" "static"))

(defconst dafny-builtins '("as" "default" "extends" "import" "include" "module" "opened" "refines" "returns" "yields"))

(defconst dafny-keywords '("assert" "assume" "break" "calc" "case" "else" "exists" "false" "forall" "fresh" "if"
                           "in" "label" "match" "modify" "new" "null" "old" "print" "return" "then" "this"
                           "true" "var" "where" "while" "yield"))

(defconst dafny-types '("array" "array2" "array3" "bool" "char" "imap" "int" "iset" "map" "multiset" "nat" "object"
                        "real" "seq" "set" "string"))

(defconst dafny-block-heads '("calc" "else" "if" "match" "while"))

(defconst dafny-all-keywords (cl-loop for source in '(dafny-defuns dafny-specifiers dafny-modifiers
                                                      dafny-builtins dafny-keywords dafny-types)
                                      append (mapcar (lambda (kwd) (propertize kwd 'source source)) (symbol-value source))))

(defconst dafny-defuns-regexp     (regexp-opt dafny-defuns 'symbols))
(defconst dafny-specifiers-regexp (regexp-opt dafny-specifiers 'symbols))
(defconst dafny-modifiers-regexp  (regexp-opt dafny-modifiers 'symbols))
(defconst dafny-builtins-regexp   (regexp-opt dafny-builtins 'symbols))
(defconst dafny-keywords-regexp   (regexp-opt dafny-keywords 'symbols))
(defconst dafny-types-regexp      (regexp-opt dafny-types 'symbols))

(defconst dafny-extended-defun-regexp (concat "\\s-*\\(" dafny-modifiers-regexp "\\)*\\s-*" dafny-defuns-regexp))

(defconst dafny-extended-block-head-regexp (concat "\\s-*\\(" dafny-modifiers-regexp "\\)*"
                                                   "\\s-*"    (regexp-opt (append dafny-block-heads dafny-defuns) 'symbols)))

(defgroup dafny nil
  "IDE extensions for the Dafny programming language."
  :group 'languages)

(defcustom dafny-snippets-repo "etc/dafny-snippets"
  "Name of file holding Dafny snippets.")

(defcustom dafny-basic-prover-args '("/enhancedErrorMessages:1" "/pretty:0" "/compile:0" "/nologo")
  "Arguments to pass to Dafny when checking a file.
The name of the file itself is added last.  You can override all
arguments here, or use `dafny-extra-prover-args' to add just a
few extra flags in addition to the default ones.")

(defcustom dafny-extra-prover-args '()
  "Extra arguments to pass to Dafny when checking a file.
These come in addition to `dafny-basic-prover-args'.")

(defconst dafny-snippets nil
  "Cache of all known Dafny snippets, loaded from `dafny-snippets-repo'.")

(defun dafny-init-snippets (&optional force-reload interactive)
  "Initialize and return `dafny-snippets'.
Reloading only happens if `dafny-snippets' is nil or if
FORCE-RELOAD is non-nil.  A non-nil INTERACTIVE value suppresses
the return value."
  (interactive '(t t))
  (setq dafny-snippets
        (or (and (not force-reload) dafny-snippets)
            (let* ((docs-fname (expand-file-name dafny-snippets-repo boogie-friends-directory))
                   (snippets (with-temp-buffer (insert-file-contents docs-fname) (buffer-string))))
              (cl-loop for index = 0 then (1+ index)
                       for line in (split-string snippets "\n\n" t)
                       for trimmed = (replace-regexp-in-string "\\(\\`\n+\\|\n+\\'\\)" "" line)
                       for cleaned = (boogie-friends-cleanup-snippet trimmed)
                       collect (propertize cleaned 'index index 'snippet trimmed)))))
  (unless interactive dafny-snippets))

(defconst dafny-font-lock-keywords ;; FIXME type constraints
  (list
   (list #'boogie-friends-mark-font-lock-assignment-chain
         1 font-lock-variable-name-face)
   (list (concat "\\(?:" dafny-defuns-regexp "\\s-+\\)+" boogie-friends-font-lock-var)
         2 font-lock-function-name-face)
   (list (concat boogie-friends-font-lock-var "\\s-*" ":" "\\s-*" boogie-friends-font-lock-type)
         '(1 font-lock-variable-name-face) '(2 font-lock-type-face prepend))
   (list "\\(\\_<forall\\_>\\).*::" 1 ''(face nil display "∀"))
   (cons dafny-defuns-regexp font-lock-builtin-face)
   (cons dafny-modifiers-regexp font-lock-preprocessor-face)
   (cons dafny-specifiers-regexp font-lock-doc-face)
   (cons dafny-builtins-regexp font-lock-builtin-face)
   (cons dafny-keywords-regexp font-lock-keyword-face)
   (cons dafny-types-regexp font-lock-type-face)
   (cons "!" font-lock-negation-char-face))
  "Font lock specifications for `dafny-mode'.")

(defun dafny-ignore-event (_e)
  "Swallow an event E.
Useful to ignore mouse-up events handled mouse-down events."
  (interactive "e"))

(defvar dafny-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'boogie-friends-verify)
    (define-key map (kbd "C-c C-a") 'dafny-show-boogie-source)
    (define-key map (kbd "C-c C-j") 'dafny-jump-to-boogie)
    (define-key map (kbd "C-c C-?") 'dafny-docs-open) ;; TODO enable by default?
    (define-key map (kbd "<C-mouse-1>") 'dafny-ignore-event)
    (define-key map (kbd "<C-S-mouse-1>") 'dafny-ignore-event)
    (define-key map (kbd "<C-down-mouse-1>") 'dafny-click-find-definition)
    (define-key map (kbd "<C-S-down-mouse-1>") 'dafny-click-jump-to-boogie)
    (define-key map (kbd "<backtab>") 'boogie-friends-cycle-indentation)
    map)
  "Keybindings for `dafny-mode'.")

(defconst dafny-mode-syntax-table
  (let ((tbl (make-syntax-table)))
    (modify-syntax-entry ?'  "w" tbl)
    (modify-syntax-entry ?_  "w" tbl)
    ;; Comments
    (modify-syntax-entry ?\n ">" tbl)
    (modify-syntax-entry ?/  "  124" tbl)
    (modify-syntax-entry ?*  "  23bn" tbl)
    tbl)
  "Syntax table for `dafny-mode'.")

(defconst dafny-boogie-proc-name "*dafny-to-boogie*"
  "Name of the Dafny → Boogie process.")

(defun dafny-boogie-buffer-name ()
  "Computes a buffer name for the Dafny → Boogie translation.
Call for the Dafny buffer!"
  (when (buffer-file-name)
    (concat (buffer-name) ".bpl")))

(defun dafny-boogie-file-name ()
  "Computes a filename for the Dafny → Boogie translation.
Call for the Dafny buffer!"
  (-when-let (fname buffer-file-name)
    (concat fname ".bpl")))

(defun dafny-boogie-filter (proc string)
  "Filter function for the Dafny → Boogie process PROC.
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

(defun dafny-boogie-sentinel (proc _sig)
  "Sentinel function for the Dafny → Boogie process PROC.
Saves the buffer upon completion of the process, and prevents the insertion
of a termination message after the conversion completes."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (when buffer-file-name (save-buffer)))
    (message (substitute-command-keys "Use \\[dafny-jump-to-boogie] or \\[dafny-click-jump-to-boogie] (Ctrl+Click) to jump to the Boogie buffer."))))

(defun dafny-get-buffer-unless-rw (buf-name)
  "Convenience wrapper around `get-buffer-create'.
Throws if a buffer of name BUF-NAME already exists and is not read-only;
otherwise, returns that buffer, or a newly created one of that
name is none is found."
  (-when-let* ((buffer (get-buffer buf-name)))
    (when (not (with-current-buffer buffer buffer-read-only))
      (error "Buffer %s already exists and is not read-only.  Cowardly refusing to overwrite it" buf-name)))
  (get-buffer-create buf-name))

(defun dafny-show-boogie-source ()
  "Translate to Boogie, save the resulting file, and display it."
  (interactive)
  (let ((buf (current-buffer)))
    (save-some-buffers nil (lambda () (eq buf (current-buffer)))))
  (-when-let* ((dfy-name  buffer-file-name)
               (buf-name  (dafny-boogie-buffer-name))
               (buf-fname (dafny-boogie-file-name))
               (buf       (dafny-get-buffer-unless-rw buf-name))
               (command   (list (flycheck-checker-executable 'dafny) dfy-name "/nologo" "/print:-" "/noVerify")))
    (-when-let* ((proc (get-buffer-process buf)))
      (ignore-errors (kill-process proc) (accept-process-output)))
    (with-current-buffer buf
      (buffer-disable-undo)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "// %s\n" (mapconcat #'identity command " "))))
      (setq buffer-file-name buf-fname)
      (boogie-mode)
      (read-only-mode))
    (display-buffer buf)
    (make-process :name dafny-boogie-proc-name
                  :buffer buf
                  :command command
                  :filter #'dafny-boogie-filter
                  :sentinel #'dafny-boogie-sentinel)))

(defun dafny-line-props ()
  "Classifies the current line (for indentation)."
  (save-excursion
    (beginning-of-line)
    (cons (cond ((or (comment-beginning) (looking-at-p "\\s-*/[/*]")) 'comment)
                ((looking-at-p "\\s-*case")                           'case)
                ((looking-at-p ".*{\\s-*\\(//.*\\)?$")                'open)
                ((looking-at-p ".*}\\s-*\\(//.*\\)?$")                'close)
                ((looking-at-p ".*;\\s-*\\(//.*\\)?$")                'semicol)
                ((looking-at-p dafny-extended-defun-regexp)           'defun)
                (t                                                    'none))
          (current-indentation))))

(defun dafny-indent ()
  "Indent current line."
  (interactive)
  (beginning-of-line)
  (let* ((pprev-type  (car (save-excursion (boogie-friends-backward-line) (boogie-friends-backward-line) (dafny-line-props))))
         (prev-props  (save-excursion (boogie-friends-backward-line) (dafny-line-props)))
         (prev-type   (car prev-props))
         (prev-offset (cdr prev-props))
         (is-defun    (looking-at-p dafny-extended-defun-regexp))
         (is-close    (looking-at-p "[^{\n]*}"))
         (is-lonely-open (looking-at-p "[ \t]*{"))
         (is-case    (looking-at-p "[ \t]*case"))
         (comment-beg (save-excursion (comment-beginning))))
    (indent-line-to
     (cond (comment-beg (if (< comment-beg (point-at-bol)) ;; Multiline comment; indent to '*' or beginning of text
                            (let ((incr (if (looking-at-p "\\s-*\\*") 1 3)))
                              (save-excursion (goto-char comment-beg) (+ (current-indentation) incr)))
                          prev-offset))
           ((or is-close is-lonely-open)
            (save-excursion
              (when is-close
                (up-list)
                (backward-sexp))
              ;; Find beginning of block head (the head can span multiple lines)
              (let ((bound (save-excursion (re-search-backward "[{}]" nil t))))
                ;; The bound ensures that brackets headerless blocks are indented properly
                (re-search-backward (concat "^\\s-*}?" dafny-extended-block-head-regexp) bound t))
              (current-indentation)))
           (is-defun (if (memq prev-type '(open)) (indent-next-tab-stop prev-offset) prev-offset))
           (is-case (or (indent-next-tab-stop (save-excursion ;; Find the parent match
                                                (when (re-search-backward "^\\s-*match" nil t) (current-indentation))))
                        prev-offset))
           (t (pcase prev-type
                (`comment prev-offset)
                (`case    (indent-next-tab-stop prev-offset))
                (`open    (indent-next-tab-stop prev-offset))
                (`close   prev-offset)
                (`semicol prev-offset)
                (`defun   (indent-next-tab-stop prev-offset))
                (`none    (if (memq pprev-type '(none defun comment)) prev-offset (indent-next-tab-stop prev-offset))))))))
  (skip-chars-forward " "))

(defun dafny-jump-to-boogie-internal (line &optional buffer)
  "Jump to translation of LINE in boogie buffer BUFFER.
Attemps to guess the right buffer if BUFFER is nil.  If unable to
find references to LINE, look for references to neighbouring
lines."
  (-when-let* ((buffer         (or buffer (-when-let* ((bpl-fname (dafny-boogie-file-name)))
                                            (find-buffer-visiting bpl-fname))))
               (window         (display-buffer buffer))
               ((dest . delta) (with-current-buffer buffer
                                 (let ((case-fold-search t))
                                   (save-excursion
                                     (cl-loop for delta in '(0 -1 -2 -3 -4 -5 1 2 3 4 5)
                                              do (goto-char (point-max))
                                              for pos = (search-backward (format ".dfy(%d," (+ line delta)) nil t)
                                              thereis (when pos (cons pos delta))))))))
    (with-current-buffer buffer
      (with-selected-window window
        (goto-char dest)
        (boogie-highlight-current-line (= 0 delta))
        (recenter)))
    delta))

(defvar-local dafny-jump-overlay nil
  "Temporary highlighting of a line matching a Boogie position.
See `dafny-jump-to-boogie'.")

(defun dafny-jump-to-boogie (line &optional buffer)
  "Jump to the Boogie location matching LINE.
Interactively, LINE is the current line.  BUFFER is the Boogie
buffer to search.  Since not all lines have a direct counterpart
in the Boogie file, the line actually matched is briefly
highlighted."
  (interactive (list (save-restriction (widen) (line-number-at-pos (point))) nil))
  (boogie-friends-clean-overlay 'dafny-jump-overlay)
  (-if-let* ((delta (dafny-jump-to-boogie-internal line buffer)))
      (progn (when (/= 0 delta)
               (message "No location found for line %d. Showing the closest available reference, %d line(s) %s."
                        line (abs delta) (if (> 0 delta) "above" "below")))
             (setq dafny-jump-overlay (save-excursion (forward-line delta)
                                                      (make-overlay (point-at-bol) (point-at-eol))))
             (overlay-put dafny-jump-overlay 'face 'highlight)
             (run-with-timer 0.5 nil #'boogie-friends-clean-overlay 'dafny-jump-overlay (current-buffer)))
    (error "No location found for line %d" line)))

(defun dafny-click-find-definition (event)
  "Find definitions of symbol under mouse pointer.
Symbol at point must be a function name. Search is restricted to
open Dafny buffers."
  (interactive "e") ;; FIXME would be much better to only show the lines below the definition
  (boogie-friends-with-click event 'dafny-mode nil
    (-when-let* ((fun-name (thing-at-point 'symbol)))
      (occur-1 (concat "^" dafny-extended-defun-regexp "\\s-*\\_<" (regexp-quote fun-name) "\\_>") 3
               (cl-loop for b being the buffers when (string-match-p "\\.dfy\\'" (buffer-name b)) collect b))
      (-when-let* ((buf (get-buffer "*Occur*")))
        (with-current-buffer buf
          (face-remap-set-base 'match '(:weight bold :inverse-video t)))))))

(defun dafny-click-jump-to-boogie (event)
  "Call `dafny-jump-to-boogie' on line under mouse."
  (interactive "e")
  (boogie-friends-with-click event 'dafny-mode t
    (dafny-jump-to-boogie (line-number-at-pos (point)) nil)))

(defun dafny-snippets-doc-buffer (arg)
  "Show documentation for snippet ARG."
  (-when-let* ((doc-buffer (dafny-docs-open))
               (doc-window (get-buffer-window doc-buffer)))
    (with-current-buffer doc-buffer
      (with-selected-window doc-window
        (save-match-data
          (when (cl-loop for regexp in '("\\_<\\(\\(?:\\w\\|\\s-\\)+\\)\\_>" "\\_<\\(\\w+\\)\\_>")
                         for needle = (when (string-match regexp arg) (match-string-no-properties 0 arg))
                         when needle thereis (progn (goto-char (point-min))
                                                    (or (re-search-forward (concat "\n  +" (regexp-quote needle)) nil t)
                                                        (search-forward needle nil t))))
            (beginning-of-line)
            (recenter))))
      (current-buffer))))

(flycheck-def-executable-var dafny "dafny")

(flycheck-define-command-checker 'dafny
  "Flycheck checker for the Dafny programming language."
  :command '("dafny" (eval (append dafny-basic-prover-args dafny-extra-prover-args)) source)
  :error-patterns boogie-friends-error-patterns
  :modes '(dafny-mode))

(add-to-list 'flycheck-checkers 'dafny)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.dfy\\'" . dafny-mode))

;;;###autoload
(define-derived-mode dafny-mode prog-mode "Dafny"
  "Major mode for editing Dafny programs.

\\{dafny-mode-map}"
  :syntax-table dafny-mode-syntax-table
  ;; (add-to-list 'boogie-friends-symbols-alist '("*" . ?×))
  (boogie-friends-mode-setup)
  (set (make-local-variable 'indent-line-function) #'dafny-indent)
  (set (make-local-variable 'indent-region-function) nil)
  (electric-indent-local-mode 1))

(provide 'dafny-mode)
;;; dafny-mode.el ends here
