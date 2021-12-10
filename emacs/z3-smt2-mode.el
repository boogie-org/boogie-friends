;;; z3-smt2-mode.el --- Support for the SMT2 format -*- lexical-binding: t -*-

;; Copyright (C) 2015, 2016 Clément Pit-Claudel
;; Author: Clément Pit-Claudel <clement.pitclaudel@live.com>
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

;; This file contains the implementation of the SMT2 part of the
;; boogie-friends package

(require 'boogie-friends)
(require 'cl-lib)

(defconst z3-smt2-builtins '("assert" "check-sat" "check-sat-assuming" "declare-const" "declare-datatype"
                             "declare-datatypes" "declare-fun" "declare-sort" "define-fun" "define-fun-rec"
                             "define-sort" "echo" "exit" "get-assertions" "get-assignment" "get-info"
                             "get-model" "get-option" "get-proof" "get-unsat-assumptions" "get-unsat-core" 
                             "get-value" "pop" "push" "reset" "reset-assertions" "set-info" "set-logic" 
                             "set-option"))

(defconst z3-smt2-types '("Array" "Set" "List" "Bool" "Int" "Real"))

(defconst z3-smt2-all-keywords (cl-loop for source in '(z3-smt2-builtins z3-smt2-types)
                                        append (mapcar (lambda (kwd) (propertize kwd 'source source)) (symbol-value source))))

(defgroup z3 nil
  "IDE extensions for the SMT2 format."
  :group 'boogie-friends)

(defcustom z3-smt2-prover-args '()
  "Arguments to pass to Z3 when checking a file.
The name of the file itself is added last.  You can override all
arguments here, or use `z3-smt2-prover-custom-args' to add just a
few extra flags in addition to the default ones."
  :group 'z3)

(defcustom z3-smt2-prover-custom-args '()
  "Extra arguments to pass to Z3 when checking a file.
These come in addition to `z3-smt2-prover-args'."
  :group 'z3)

(defcustom z3-smt2-prover-background-args '()
  "Extra arguments to pass to Z3 for background verification.
These come in addition to `z3-smt2-prover-args' and
`z3-smt2-prover-custom-args'."
  :group 'z3)

(defcustom z3-smt2-prover-local-args '()
  "Extra arguments to pass to Z3 when checking a file.
These come in addition to `z3-smt2-prover-args' and
`z3-smt2-prover-custom-args'."
  :group 'z3)

(defcustom z3-smt2-prover-alternate-args '()
  "Extra arguments to pass to Z3 when compiling with a prefix arg.
Added to `z3-smt2-prover-basic-args' and `z3-smt2-prover-custom-args'
when launching manual verification (\\[z3-smt2-friends-verify])
with a prefix arg."
  :group 'z3)

(defvar z3-smt2-font-lock-keywords
  (list
   (cons "!" font-lock-negation-char-face)
   (cons (regexp-opt z3-smt2-types 'symbols) font-lock-type-face)
   (cons (regexp-opt z3-smt2-builtins 'symbols) font-lock-builtin-face))
  "Font lock specifications for `z3-smt2-mode'.")

(defvar z3-smt2-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'boogie-friends-verify)
    map)
  "Keybindings for `z3-smt2-mode'.")

(defvar z3-smt2-mode-syntax-table
  lisp-mode-syntax-table
  "Syntax table used in `z3-smt2-mode'.")

(defun z3-smt2-syntactic-face-function (state)
  (let ((lisp-font (lisp-font-lock-syntactic-face-function state)))
    (or lisp-font (and (nth 3 state)
                       (eq (char-after (nth 8 state)) ?|)
                       font-lock-string-face))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.smt2\\'" . z3-smt2-mode))

(boogie-friends-def-exec z3 "z3.exe" "z3")

(defconst z3-smt2-error-patterns
  '((error bol "(error \"line " line " column " column ":" (message) "\")" eol)
    (error bol "ERROR: line " line " column " column ":" (message)  eol)
    (warning bol "WARNING: " (message)  eol)))

(flycheck-define-command-checker 'z3-smt2
  "Flycheck checker for the SMT2 format."
  :command '("z3" (eval (boogie-friends-compute-prover-args)) source-inplace)
  :error-patterns z3-smt2-error-patterns
  :error-filter (lambda (errors)
                  (flycheck-sanitize-errors
                   (flycheck-increment-error-columns errors)))
  :modes '(z3-smt2-mode))

(add-to-list 'flycheck-checkers 'z3-smt2)

;;;###autoload
(define-derived-mode z3-smt2-mode prog-mode "Z3-SMT2"
  "Major mode for editing SMT2 programs.

\\{z3-smt2-mode-map}"
  :group 'lisp
  :syntax-table z3-smt2-mode-syntax-table

  (lisp-mode-variables nil nil)
  (setq font-lock-defaults
        (cl-subst 'z3-smt2-syntactic-face-function
                  'lisp-font-lock-syntactic-face-function font-lock-defaults))
  (font-lock-add-keywords nil z3-smt2-font-lock-keywords)

  ;; FIXME find a way to limit Z3 error count (typing [| generates lots of errors
  (set (make-local-variable 'flycheck-checker-error-threshold) nil)
  (boogie-friends-mode-setup t)
  (boogie-friends-setup-prettify))

(provide 'z3-smt2-mode)
;;; z3-smt2-mode.el ends here
