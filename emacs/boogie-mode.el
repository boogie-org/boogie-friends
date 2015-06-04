;;; boogie-mode.el --- Support for the Boogie programming language -*- lexical-binding: t -*-

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

;; This file contains the implementation of the Boogie part of the
;; boogie-friends package

(require 'boogie-friends)

(defconst boogie-builtins '("axiom" "complete" "const" "ensures" "extends" "free" "function" "implementation"
                            "invariant" "modifies" "procedure" "requires" "returns" "type" "unique" "var" "where"))

(defconst boogie-keywords '("assert" "assume" "break" "call" "cast" "div" "else" "exists" "false" "forall" "goto"
                            "havoc" "if" "lambda" "mod" "old" "return" "then" "true" "while"))

(defconst boogie-all-keywords (cl-loop for source in '(boogie-builtins boogie-keywords)
                                       append (mapcar (lambda (kwd) (propertize kwd 'source source)) (symbol-value source))))

(defconst boogie-checker-extra-args
  '(list "/z3opt:TRACE=true") ; (concat "/z3opt:TRACE_FILE_NAME='" (or buffer-file-name "log") "-z3.log'"))
  "Extra flags passed to Boogie when compiling with a prefix arg (\\[boogie-friends-verify])")

(defvar boogie-font-lock-keywords
  (let ((sb "\\(?:\\sw\\|\\s_\\|[<>]\\)+"))
    (list
     (cons "!"
           font-lock-negation-char-face)
     (cons "\\_<T[A-Z]\\sw+\\_>"
           font-lock-type-face)
     (cons (regexp-opt boogie-builtins 'symbols)
           font-lock-builtin-face)
     (cons (regexp-opt boogie-keywords 'symbols)
           font-lock-keyword-face)
     (cons (concat "\\(" (regexp-opt '("bool" "int" "real") 'symbols) "\\)\\|\\(\\_<bv[0-9]+\\_>\\)")
           font-lock-type-face)
     (list (concat "\\(\\_<\\(" sb "\\)\\_>\\)(")
           '(1 font-lock-function-name-face append))
     (list (concat "\\_<\\(" sb "\\)\\_>" "\\s-*" ":" "\\s-*" "\\_<\\(" sb "\\)\\_>")
           '(1 font-lock-variable-name-face) '(2 font-lock-type-face))
     (list "{:[^{\n]+}"
           '(0 font-lock-constant-face append))
     (list "\\({\\s-*\\)\\([^{\n]+?\\)\\(\\s-*}\\)"
           '(1 font-lock-constant-face) '(2 '(face italic) prepend) '(3 font-lock-constant-face))))
  "Font lock specifications for `boogie-mode'.")

(defvar boogie-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'boogie-friends-verify)
    (define-key map (kbd "<backtab>") 'boogie-friends-cycle-indentation)
    map)
  "Keybindings for `boogie-mode'.")

(defvar boogie-mode-syntax-table
  (let ((tbl (make-syntax-table)))
    (modify-syntax-entry ?' "w" tbl)
    (modify-syntax-entry ?_ "w" tbl)
    (modify-syntax-entry ?# "_" tbl)
    (modify-syntax-entry ?$ "_" tbl)
    (modify-syntax-entry ?. "_" tbl)
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
  "Flycheck checker for the Boogie programming language."
  :command '("" "/nologo" source)
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
;;; boogie-mode.el ends here
