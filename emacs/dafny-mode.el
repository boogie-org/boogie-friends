;;; dafny-mode.el - Support for Dafny in Emacs

(require 'boogie-friends)
(require 'boogie-mode)
(require 'dafny-docs)

(defconst dafny-defuns '("class" "codatatype" "colemma" "constructor" "copredicate" "datatype" "function" "inductive"
                         "iterator" "lemma" "method" "newtype" "predicate" "trait" "type"))

(defconst dafny-specifiers '("decreases" "ensures" "free" "invariant" "modifies" "reads" "requires"))

(defconst dafny-modifiers '("abstract" "ghost" "protected" "static"))

(defconst dafny-builtins '("var"
                           "module" "import" "default" "as" "opened"
                           "include"
                           "extends" "refines" "returns" "yields"))

(defconst dafny-keywords '("assert" "assume" "break" "else" "if" "label" "return" "then" "yield"
                           "print" "where" "while"
                           "calc" "exists" "forall" "fresh" "in" "modify" "new" "old" "this"
                           "case" "false" "match" "null" "true"))

(defconst dafny-types '("array" "array2" "array3" "bool" "char"
                        "imap" "int" "map" "multiset" "nat" "object"
                        "real" "seq" "set" "string"))

(defconst dafny-all-keywords (cl-loop for source in '(dafny-defuns dafny-specifiers dafny-modifiers
                                                                        dafny-builtins dafny-keywords dafny-types)
                                           append (mapcar (lambda (kwd) (propertize kwd 'source source)) (symbol-value source))))

(defconst dafny-snippets nil)

(defun dafny-init-snippets (&optional force-reload interactive)
  (interactive '(t t))
  (setq dafny-snippets
        (or (and (not force-reload) dafny-snippets)
            (let* ((docs-fname (expand-file-name "dafny-snippets" boogie-friends-directory))
                   (snippets (with-temp-buffer (insert-file-contents docs-fname) (buffer-string))))
              (cl-loop for index = 0 then (1+ index)
                       for line in (split-string snippets "\n\n" t)
                       for trimmed = (replace-regexp-in-string "\\(\\`\n+\\|\n+\\'\\)" "" line)
                       for cleaned = (boogie-friends-cleanup-snippet trimmed)
                       collect (propertize cleaned 'index index 'snippet trimmed)))))
  (unless interactive dafny-snippets))

(defconst dafny-font-lock-keywords
  (let ((sb "\\_<\\(\\(?:\\sw\\|[<>]\\)+\\)\\_>"))
    (list
     (list (concat "\\(?:" (regexp-opt dafny-defuns 'symbols) "\\s-+\\)+" sb)
           2 font-lock-function-name-face)
     (list (concat sb "\\s-*" ":" "\\s-*" sb)
           '(1 font-lock-variable-name-face) '(2 font-lock-type-face))
     '("\\_<var\\_>" (0 font-lock-keyword-face)
       ("\\_<\\(?:\\sw\\|\\s_\\)+\\_>"
        (save-excursion (save-match-data (when (search-forward ":=" (point-at-eol) t) (match-beginning 0)))) nil
        (0 font-lock-variable-name-face)))
     (list (concat "\\(\\(?:" sb ",?\\)+\\)\\s-*" ":=")
           1 font-lock-constant-face)
     (list "\\(\\_<forall\\_>\\).*::"
           1 ''(face nil display "∀"))
     (cons (regexp-opt dafny-defuns 'symbols) font-lock-builtin-face)
     (cons (regexp-opt dafny-modifiers 'symbols) font-lock-preprocessor-face)
     (cons (regexp-opt dafny-specifiers 'symbols) font-lock-doc-face)
     (cons (regexp-opt dafny-builtins 'symbols) font-lock-builtin-face)
     (cons (regexp-opt dafny-keywords 'symbols) font-lock-keyword-face)
     (cons (regexp-opt dafny-types 'symbols) font-lock-type-face))))

(defun dafny-ignore-event (e)
  (interactive "e"))

(defvar dafny-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'boogie-friends-verify)
    (define-key map (kbd "C-c C-a") 'dafny-show-boogie-source)
    (define-key map (kbd "C-c C-j") 'dafny-jump-to-boogie)
    (define-key map (kbd "C-c C-?") 'dafny-docs-open) ;; TODO enable by default?
    (define-key map (kbd "<C-mouse-1>") 'dafny-ignore-event)
    (define-key map (kbd "<C-down-mouse-1>") 'dafny-click-jump-to-boogie)
    (define-key map (kbd "<backtab>") 'dafny-cycle-indentation)
    map)
  "Dafny mode's keymap")

(defconst dafny-mode-syntax-table
  (let ((tbl (make-syntax-table)))
    (modify-syntax-entry ?'  "w" tbl)
    (modify-syntax-entry ?_  "w" tbl)
    ;; Comments
    (modify-syntax-entry ?\n ">" tbl)
    (modify-syntax-entry ?/  "  124" tbl)
    (modify-syntax-entry ?*  "  23bn" tbl)
    tbl))

(defconst dafny-boogie-proc-name "*boogie*")

(defun dafny-buffer-boogie-names ()
  (-when-let* ((fname (buffer-file-name)))
    (cons (concat (buffer-name) ".bpl") (concat fname ".bpl"))))

(defun dafny-boogie-filter (proc string)
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (process-mark proc))
          (insert string)
          (set-marker (process-mark proc) (point)))
        (goto-char (point-min))))))

(defun dafny-boogie-sentinel (proc sig) ;; Prevent insertion of termination messages
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (when buffer-file-name (save-buffer)))
    (message (substitute-command-keys "Use \\[dafny-jump-to-boogie] or \\[dafny-click-jump-to-boogie] (Ctrl+Click) to jump to the Boogie buffer."))))

(defun dafny-show-boogie-source ()
  (interactive)
  (let ((buf (current-buffer)))
    (save-some-buffers nil (lambda () (eq buf (current-buffer)))))
  (-when-let* ((dfy-name  buffer-file-name)
               (bpl-names (dafny-buffer-boogie-names))
               (buf       (get-buffer-create (car bpl-names)))
               (command   (list (flycheck-checker-executable 'dafny) dfy-name "/nologo" "/print:-" "/noVerify")))
    (-when-let ((proc (get-buffer-process buf)))
      (ignore-errors (kill-process proc)
                     (accept-process-output)))
    (with-current-buffer buf
      (buffer-disable-undo)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "// %s\n" (mapconcat #'identity command " "))))
      (setq buffer-file-name (cdr bpl-names))
      (boogie-mode)
      (read-only-mode))
    (display-buffer buf)
    (make-process :name dafny-boogie-proc-name
                  :buffer buf
                  :command command
                  :filter #'dafny-boogie-filter
                  :sentinel #'dafny-boogie-sentinel)))

(defun dafny-backward-line ()
  (forward-line 0)
  (skip-chars-backward "\r\n\t "))

;; (defun dafny-real-line-contents ()
;;   (save-excursion
;;     (let ((eol (point-at-eol)))
;;       (cl-loop do (beginning-of-line)
;;                for cb = (comment-beginning)
;;                while cb do (goto-char cb))
;;       (cl-loop do (cl-loop do (skip-chars-forward " \t")
;;                            while (not (eq (point) (progn (comment-forward) (point)))))
;;                while (< (point) eol)
;;                collect (buffer-substring-no-properties (point) (goto-char (or (comment-search-forward eol t) eol)))))))

(defun dafny-cycle-indentation (&optional rev)
  (interactive)
  (let ((cur  (current-indentation))
        (prev (save-excursion (dafny-backward-line) (current-indentation))))
    (if rev
        (indent-line-to (if (= cur 0) (indent-next-tab-stop prev) (indent-next-tab-stop cur rev)))
      (indent-line-to (if (> cur prev) 0 (indent-next-tab-stop cur rev))))))

;; (defun dafny-cycle-indentation-rev ()
;;   (interactive)
;;   (dafny-cycle-indentation t))

;; (defun dafny-count (what from to)
;;   (save-excursion
;;     (save-match-data
;;       (goto-char from)
;;       (cl-loop while (search-forward what to t) sum 1))))

;; (defun dafny-line-indentedness ()
;;   (- (dafny-count "{" (point-at-bol) (point-at-eol))
;;      (dafny-count "}" (point-at-bol) (point-at-eol))))

(defconst dafny-defun-regexp    (concat "\\s-*\\(" (regexp-opt dafny-modifiers 'symbols) "\\)*"
                                        "\\s-*"    (regexp-opt dafny-defuns    'symbols)))

(defconst dafny-block-heads '("while" "if" "else" "match" "calc"))

(defconst dafny-block-hd-regexp (concat "\\s-*\\(" (regexp-opt dafny-modifiers 'symbols) "\\)*"
                                        "\\s-*"    (regexp-opt (append dafny-block-heads dafny-defuns) 'symbols)))

(defun dafny-line-props ()
  (save-excursion
    (beginning-of-line)
    (cons (cond ((or (comment-beginning) (looking-at-p "\\s-*/[/*]")) 'comment)
                ((looking-at-p "\\s-*case")                           'case)
                ((looking-at-p ".*{\\s-*\\(//.*\\)?$")                'open)
                ((looking-at-p ".*}\\s-*\\(//.*\\)?$")                'close)
                ((looking-at-p ".*;\\s-*\\(//.*\\)?$")                'semicol)
                ((looking-at-p dafny-defun-regexp)                    'defun)
                (t                                                    'none))
          (current-indentation))))

(defun dafny-indent ()
  (interactive)
  (beginning-of-line)
  (let* ((pprev-type  (car (save-excursion (dafny-backward-line) (dafny-backward-line) (dafny-line-props))))
         (prev-props  (save-excursion (dafny-backward-line) (dafny-line-props)))
         (prev-type   (car prev-props))
         (prev-offset (cdr prev-props))
         (is-defun    (looking-at-p dafny-defun-regexp))
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
                (re-search-backward (concat "^\\s-*}?" dafny-block-hd-regexp) bound t))
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
  (-when-let* ((buffer         (or buffer (let ((bpl-fname (cdr-safe (dafny-buffer-boogie-names))))
                                            (when bpl-fname (find-buffer-visiting bpl-fname)))))
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

(defvar-local dafny-jump-overlay nil)

(defun dafny-jump-to-boogie (line &optional buffer)
  (interactive (list (save-restriction (widen) (line-number-at-pos (point))) nil))
  (boogie-friends-clean-overlay 'dafny-jump-overlay)
  (let ((delta (dafny-jump-to-boogie-internal line buffer)))
    (when (/= 0 delta)
      (message "No location found for line %d. Showing the closest available reference, %d line(s) %s."
               line (abs delta) (if (> 0 delta) "above" "below")))
    (setq dafny-jump-overlay (save-excursion (forward-line delta) (make-overlay (point-at-bol) (point-at-eol))))
    (overlay-put dafny-jump-overlay 'face 'highlight)
    (run-with-timer 0.5 nil #'boogie-friends-clean-overlay 'dafny-jump-overlay (current-buffer))))

(defun dafny-click-jump-to-boogie (event)
  (interactive "e")
  (mouse-set-point event)
  (-when-let* ((window  (posn-window (event-start event)))
             (buffer  (window-buffer window)))
      (with-selected-window window
        (with-current-buffer buffer
          (when (eq major-mode 'dafny-mode)
            (dafny-jump-to-boogie (line-number-at-pos (point)) nil))))))

(defun dafny-snippets-doc-buffer (arg)
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
  "A Dafny checker."
  :command '("" "/enhancedErrorMessages:1" "/pretty:0" "/compile:0" "/nologo" source)
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

;; FIXME warn if checker not found

(provide 'dafny-mode)
