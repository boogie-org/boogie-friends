;;; -*- lexical-binding: t -*-
;; boogie-mode.el - Support for Boogie in Emacs

(require 'boogie-friends)

(defconst boogie-builtins '("axiom" "complete" "const" "ensures" "extends" "free" "function" "implementation"
                            "invariant" "modifies" "procedure" "requires" "returns" "type" "unique" "var" "where"))

(defconst boogie-keywords '("assert" "assume" "break" "call" "then" "else" "havoc" "if" "goto" "return" "while"
                            "old" "forall" "exists" "lambda" "cast" "div" "mod" "false" "true"))

(defconst boogie-all-keywords (cl-loop for source in '(boogie-builtins boogie-keywords)
                                            append (mapcar (lambda (kwd) (propertize kwd 'source source)) (symbol-value source))))

(defvar boogie-font-lock-keywords
  (let ((sb "\\(?:\\sw\\|[<>]\\)+"))
    (list
     (cons "{ [^{\n]+ }"
           font-lock-preprocessor-face)
     (list "{:[^{\n]+}"
           0 font-lock-constant-face 'append)
     (cons "\\_<T[A-Z]\\sw+\\_>"
           font-lock-type-face)
     (list (concat "\\(\\_<\\(" sb "#" sb "\\|\\$" sb "\\)\\_>\\)(")
           1 font-lock-function-name-face)
     (list (concat "\\_<\\(" sb "\\)\\_>" "\\s-*" ":" "\\s-*" "\\_<\\(" sb "\\)\\_>")
           '(1 font-lock-variable-name-face) '(2 font-lock-type-face))
     (cons (regexp-opt boogie-builtins 'symbols)
           font-lock-builtin-face)
     (cons (regexp-opt boogie-keywords 'symbols)
           font-lock-keyword-face)
     (cons (concat "\\(" (regexp-opt '("bool" "int" "real") 'symbols) "\\)\\|\\(\\_<bv[0-9]+\\_>\\)")
           font-lock-type-face)))
  "Font lock specifications for `boogie-mode'.")

(defvar boogie-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'boogie-friends-verify)
    map)
  "Keybindings for `boogie-mode'.")

(defvar boogie-mode-syntax-table
  (let ((tbl (make-syntax-table)))
    (modify-syntax-entry ?' "w" tbl)
    (modify-syntax-entry ?_ "w" tbl)
    ;; Comments
    (modify-syntax-entry ?\n ">" tbl)
    (modify-syntax-entry ?/  "  124" tbl)
    (modify-syntax-entry ?*  "  23bn" tbl)
    tbl)
  "Syntax table for `boogie-mode'.")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.bpl\\'" . boogie-mode))

(flycheck-def-executable-var boogie "boogie")

(flycheck-define-command-checker 'boogie
  "Flycheck checker for the Dafny programming language."
  :command '("" "/nologo" "/abbrevOutput" source)
  :error-patterns boogie-friends-error-patterns
  :modes '(boogie-mode))

(defvar-local boogie-highlighting-overlay nil
  "Temporary highlighting of a line matching a Dafny position.
See `dafny-jump-to-boogie'.")

(defun boogie-highlight-current-line (_exact)
  "Temporarily highlight the current line.
Existing highlights are suppressed."
  (interactive)
  (boogie-friends-clean-overlay 'dafny-jump-overlay)
  (boogie-friends-clean-overlay nil 'boogie-highlighting-overlay)
  (setq boogie-highlighting-overlay (make-overlay (point-at-bol) (point-at-eol)))
  (overlay-put boogie-highlighting-overlay 'face 'highlight)
  (run-with-timer 0.5 nil #'boogie-friends-clean-overlay 'boogie-highlighting-overlay (current-buffer)))

;;;###autoload
(define-derived-mode boogie-mode prog-mode "Boogie"
  "Major mode for editing Boogie programs.

\\{boogie-mode-map}"
  :syntax-table boogie-mode-syntax-table
  (add-to-list 'boogie-friends-symbols-alist '("forall" . ?∀))
  (boogie-friends-mode-setup))

(provide 'boogie-mode)
