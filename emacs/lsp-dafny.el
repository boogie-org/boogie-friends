;;; lsp-dafny.el --- LSP client for Dafny -*- lexical-binding: t -*-

;; Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
;; SPDX-License-Identifier: Apache-2.0
;; Author: Clément Pit-Claudel <clement.pitclaudel@live.com>
;; URL: https://github.com/boogie-org/boogie-friends/
;; Keywords: convenience, languages
;; This file is not part of GNU Emacs.

;;; Commentary:

;; See boogie-friends.el for general information about this package.
;;
;; This file implements a client for the Dafny language server server.

;;; Code:

(require 'lsp-mode)
(require 'lsp-protocol)
(require 'lsp-diagnostics)
(require 'company-capf)
(require 'boogie-friends)

;;; Customization

(defgroup lsp-dafny nil
  "Options for dafny-mode's LSP support."
  :group 'lsp
  :prefix "lsp-dafny"
  :link '(url-link "https://github.com/dafny-lang/dafny/"))

;;;; Installation options

(defconst lsp-dafny-latest-known-version "4.3.0")

(defun lsp-dafny--version-safe-p (vernum)
  "Check whether VERNUM is a safe value for `lsp-dafny-preferred-version'."
  (or (null vernum)
      (stringp vernum)))

(defcustom lsp-dafny-preferred-version nil
  "Which version of Dafny to run."
  :safe #'lsp-dafny--version-safe-p
  :type '(choice (const :tag "Auto-install the latest version" nil)
                 (choice :tag "Auto-install a specific version"
                         (const "4.3.0")
                         (const "4.2.0")
                         (const "4.1.0")
                         (const "4.0.0")
                         (const "3.13.0")
                         (const "3.12.0")
                         (const "3.11.0")
                         (const "3.10.0")
                         (const "3.9.0")
                         (const "3.8.1")
                         (const "3.8.0")
                         (const "3.7.3")
                         (const "3.7.2")
                         (const "3.7.1")
                         (const "3.7.0")
                         (const "3.6.0")
                         (const "3.5.0")
                         (const "3.4.2")
                         (const "3.4.1")
                         (const "3.4.0")
                         (const "3.3.0")
                         (const "3.2.0")
                         (const "3.1.0")
                         (const "3.0.0")
                         (string :tag "Other version"))
                 (const :tag "Find Dafny in your PATH" path)
                 (list :tag "Use a custom Dafny installation"
                       (directory :tag "Dafny installation directory"))))

(defcustom lsp-dafny-server-install-root
  (expand-file-name "dafny" lsp-server-install-dir)
  "Where to install Dafny servers."
  :risky t
  :type 'directory)

;;;; Server options

(defcustom lsp-dafny-server-automatic-verification-policy 'onchange
  "When to verify Dafny code."
  :type '(choice (const :tag "Never trigger verification automatically" never)
                 (const :tag "Verify on change" onchange)
                 (const :tag "Verify on save" onsave)))

(defcustom lsp-dafny-server-verification-time-limit 10
  "How long to search for a proof before giving up."
  :type '(choice (const :tag "No limit" 0)
                 (integer :tag "Give up after this many seconds")))

;; TODO "--ghost:markStatements=true"

(defcustom lsp-dafny-server-args '("--verifier:verifySnapshots=3")
  "Dafny language server arguments."
  :risky t
  :type '(repeat string))

;;;; Faces

(defface lsp-dafny-counterexample-border
  '((((class color) (min-colors 88))
     :background "grey65" :weight bold :extend t)
    (t :inverse-video t :extend t))
  "Face used for counterexample borders.")

(defface lsp-dafny-counterexample-separator
  '((t :extend t))
  "Face used for counterexample separators.")

(defface lsp-dafny-counterexample-inset
  '((((class color) (min-colors 88) (background light))
     :background "grey90" :height 0.8 :extend t)
    (((class color) (min-colors 88) (background dark))
     :background "grey30" :height 0.8 :extend t)
    (t :slant italic :extend t))
  "Face used for counterexample insets.")

;;; Macros

(defmacro lsp-dafny--with-whole-buffer-unmodified (&rest body)
  "Run BODY on the whole buffer without tracking changes or position."
  (declare (debug t) (indent 0))
  `(with-silent-modifications
     (save-excursion
       (save-restriction
         (widen)
         ,@body))))

;;; LSP setup

;;;; Server installation

(defun lsp-dafny-resolve-preferred-version ()
  "Validate `lsp-dafny-preferred-version', returning a default if unset.

Returns a value permitted by `lsp-dafny-preferred-version', but
not nil."
  (pcase (or lsp-dafny-preferred-version lsp-dafny-latest-known-version)
    (`path `path)
    ((and v (pred stringp))
     (condition-case err
         (progn (version-to-list v) v)
       (err (user-error "Invalid `lsp-dafny-preferred-version': %S
%S" v (cdr err)))))
     (`(,v)
      (if (file-exists-p v) (list v)
        (user-error "Invalid `lsp-dafny-preferred-version': %S
Directory %s does not exist" v)))
     (_ (user-error "Invalid `lsp-dafny-preferred-version': %S"))))

(defun lsp-dafny-server-install-dir (&optional vernum)
  "Compute the path to Dafny's installation folder for version VERNUM.

VERNUM defaults to `lsp-dafny-preferred-version'.  Returns either
a directory name, or nil if VERNUM is `path'."
  (pcase (or vernum (lsp-dafny-resolve-preferred-version))
    ((and v (pred stringp))
     (expand-file-name (format "v%s" v) lsp-dafny-server-install-root))
    (`path `path)
    (`(,v) v)))

(defun lsp-dafny--installed-executable (executable)
  "Compute the path to an installed Dafny EXECUTABLE."
  (pcase (lsp-dafny-server-install-dir)
    (`path executable)
    (dir (expand-file-name executable dir))))

(defun lsp-dafny--server-installed-executable ()
  "Compute the path to the installed version of DafnyLanguageServer."
  (lsp-dafny--installed-executable "DafnyLanguageServer"))

(defun lsp-dafny--zip-url (vernum)
  "Compute the URL to download Dafny version VERNUM."
  (let ((platform
         (pcase system-type
           ((or `gnu `gnu/linux) (if (string-version-lessp vernum "3.13.0") "ubuntu-16.04" "ubuntu-20.04"))
           ((or `windows-nt `cygwin) (if (string-version-lessp vernum "3.13.0") "win" "windows-2019"))
           ((or `darwin) (if (string-version-lessp vernum "3.13.0") "osx-10.14.2" "macos-11"))
           (other "Unsupported platform %S" other))))
    (format "%s/v%s/dafny-%s-x64-%s.zip"
            "https://github.com/dafny-lang/dafny/releases/download"
            vernum vernum platform)))

(defun lsp-dafny--server-install-callback (root vernum callback)
  "Install Dafny version VERNUM from ROOT/dafny and call CALLBACK."
  (condition-case _
      (rename-file (expand-file-name "dafny" root)
                   (lsp-dafny-server-install-dir vernum))
    (file-already-exists nil))
  (funcall callback))

(defun lsp-dafny--server-install (client callback error-callback update?)
  "Download Dafny and install it to `lsp-dafny-server-install-dir'.

Version installed is determined by `lsp-dafny-preferred-version'.

Call CALLBACK on success; call ERROR-CALLBACK otherwise.
CLIENT and UPDATE? are ignored."
  (ignore client update?)
  (let* ((root lsp-dafny-server-install-root)
         (vernum (lsp-dafny-resolve-preferred-version))
         (dl-dir (lsp-dafny-server-install-dir vernum)))
    (make-directory dl-dir t)
    (lsp-download-install
     (apply-partially
      #'lsp-dafny--server-install-callback root vernum callback)
     error-callback
     :url (lsp-dafny--zip-url vernum)
     :store-path dl-dir
     :decompress :zip)))

;;;; Overrides

(defun lsp-dafny-diagnostics--flycheck-calculate-level (severity tags)
  "Return `tooltip' if SEVERITY is 4, nil otherwise.
TAGS are ignored."
  (ignore tags)
  (and (bound-and-true-p lsp-dafny--mode)
       (equal severity 4)
       'tooltip))


;;; Additional behaviors

;;;; Minor mode

(defconst lsp-dafny-modules
  `(lsp-dafny-counterexamples-module
    lsp-dafny-mode-line-status-module
    lsp-dafny-company-module
    lsp-dafny-flycheck-module
    lsp-dafny-trace-module))

(defconst lsp-dafny-disabled-modules
  `(lsp-dafny-trace-module))

(defun lsp-dafny-enabled-modules ()
  "Collect all enabled LSP-Dafny modules."
  (cons 'lsp-dafny-hooks-module
        (cl-set-difference lsp-dafny-modules
                           lsp-dafny-disabled-modules)))

(defun lsp-dafny--toggle-modules (command)
  "Run all LSP Dafny modules with COMMAND."
  (dolist (mod (lsp-dafny-enabled-modules))
    (funcall mod command)))

(define-minor-mode lsp-dafny--mode
  "Minor mode to manage LSP-Dafny features."
  :init-value nil
  (lsp-dafny--toggle-modules (if lsp-dafny--mode 'on 'off)))

(defun lsp-dafny-teardown ()
  "Deactivate all LSP Dafny features."
  (lsp-dafny--mode -1))

;;;; Hooks

(defun lsp-dafny-hooks-module (command)
  "Set up Dafny LSP hooks according to COMMAND."
  (pcase command
    (`on
     (add-hook 'change-major-mode-hook #'lsp-dafny-teardown nil t)
     (add-hook 'before-revert-hook #'lsp-dafny-teardown nil t)
     (add-hook 'kill-buffer-hook #'lsp-dafny-teardown nil t))
    (`off
     (remove-hook 'change-major-mode-hook #'lsp-dafny-teardown t)
     (remove-hook 'before-revert-hook #'lsp-dafny-teardown t)
     (remove-hook 'kill-buffer-hook #'lsp-dafny-teardown t))))

;;;; Company setup

(defun lsp-dafny-company-module (command)
  "Set up company for use with Dafny LSP according to COMMAND."
  (pcase command
    (`on
     (setq-local company-backends
                 (cons #'company-capf
                       (remove #'company-capf company-backends))))
    (`off
     (setq-local company-backends
                 (remove #'company-capf company-backends)))))

;;;; Flycheck setup

(defun lsp-dafny-flycheck-module (command)
  "Set up Flycheck for use with Dafny LSP according to COMMAND."
  (pcase command
    (`on
     (advice-add #'lsp-diagnostics--flycheck-calculate-level
                 :before-until #'lsp-dafny-diagnostics--flycheck-calculate-level))
    (`off
     (advice-remove #'lsp-diagnostics--flycheck-calculate-level
                    #'lsp-dafny-diagnostics--flycheck-calculate-level))))

;;;; Custom notifications

(lsp-interface (dafny:compilation/progress (:uri :status :message)))

(defvar-local lsp-dafny--verification-status nil)

(defvar lsp-dafny--verification-status-changed-hook ()
  "Hook run when Dafny's verification status changes.")

(defun lsp-dafny--handle-/compilation/status (_workspace msg)
  "Handle the dafny/compilation/status notification MSG."
  (pcase-let (((dafny:compilation/progress :uri :status :message) msg))
    (with-current-buffer (-> uri lsp--uri-to-path find-file-noselect)
      (setq-local
       lsp-dafny--verification-status
       (pcase status
         ("ParsingFailed"
          '(failure "!" "Parsing error"))
         ("ResolutionFailed"
          '(failure "!" "Resolution error"))
         ("ResolutionStarted"
          '(running " " "Resolution in progress"))
         ("CompilationSucceeded"
          '(running " " "Resolved but not verified yet"))
         ("VerificationStarted"
          (if message
              `(running ,(format "[%s…]" message) "Verification in progress")
            '(running "…" "Verification started")))
         ("VerificationFailed"
          '(failure "✗" "Verification failed"))
         ((or "VerificationSucceeded")
          '(success "✓" "Verification succeeded"))
         (other
          `(running ,other (format "Unknown status (%S)" other)))))
      (run-hook-with-args 'lsp-dafny--verification-status-changed-hook))))

;;;; Modeline progress indicator

(defun lsp-dafny--mode-line-on-change (&rest _args)
  "Reset verification progress."
  (when lsp-dafny--verification-status
    (setq lsp-dafny--verification-status nil)
    (run-hook-with-args 'lsp-dafny--verification-status-changed-hook)))

(defun lsp-dafny--mode-line-update ()
  "Force a mode line update."
  (force-mode-line-update))

(defun lsp-dafny--mode-line-process-format ()
  "Compute text to display in Dafny's mode-line."
  (pcase-let ((`(,status ,icon ,tooltip)
               (or lsp-dafny--verification-status
                   '(waiting " " "Verification not started"))))
    (let ((face (pcase status
                  (`waiting nil)
                  (`running '(bold compilation-mode-line-run))
                  (`success '(bold compilation-mode-line-exit))
                  (`failure '(bold compilation-mode-line-fail)))))
      (propertize (if (eq (length icon) 1)
                      (compose-string icon 0 1 `[#x2003 (Bl . Bl) ,(aref icon 0)])
                    ;; '(concat "\t" icon "\t"))
                    icon)
                  'face face
                  'help-echo tooltip))))

(defconst lsp-dafny--mode-line-process
  '(:eval (lsp-dafny--mode-line-process-format)))
(put 'lsp-dafny--mode-line-process 'risky-local-variable t)

(defun lsp-dafny--test-mode-line-process ()
  "Check if `mode-line-process' contains our marker.

See URL `https://github.com/Malabarba/spinner.el/issues/26' for
details."
  (let* ((sym (gensym))
         (lsp-dafny--mode-line-process `(:eval (setq ,sym t))))
    (ignore-errors
      (set sym nil)
      (format-mode-line mode-line-process)
      (symbol-value sym))))

(defun lsp-dafny-mode-line-status-module (command)
  "Set up text-based verification progress for LSP Dafny according to COMMAND."
  (pcase command
    (`on
     (make-local-variable 'mode-line-process)
     (if mode-line-process
         (unless (lsp-dafny--test-mode-line-process)
           (add-to-list 'mode-line-process 'lsp-dafny--mode-line-process t))
       (setq mode-line-process lsp-dafny--mode-line-process))
     (add-hook 'after-change-functions #'lsp-dafny--mode-line-on-change nil t)
     (add-hook 'lsp-dafny--verification-status-changed-hook
               #'lsp-dafny--mode-line-update))
    (`off
     (setq-local mode-line-process
                 (delq 'lsp-dafny--mode-line-process mode-line-process))
     (remove-hook 'after-change-functions #'lsp-dafny--mode-line-on-change t)
     (remove-hook 'lsp-dafny--verification-status-changed-hook
                  #'lsp-dafny--mode-line-update))))

;;;; Counterexamples

(lsp-interface (dafny:counterexample (:position :variables)))

(defvar-local lsp-dafny--counterexamples ()
  "Local cache of counterexample windows.")

(defun lsp-dafny--counterexamples-hide ()
  "Hide counterexamples in the current buffer."
  (lsp-dafny--with-whole-buffer-unmodified
    (mapc #'delete-overlay lsp-dafny--counterexamples)))

(defconst lsp-dafny--counterexamples-border
  (concat
   (propertize " " 'face 'lsp-dafny-counterexample-border
               'display `(space :width 0.35 :height (1)))
   (propertize " " 'display `(space :width 0.65 :height (1)))))

(defconst lsp-dafny--counterexamples-separator
  (propertize "\n" 'face 'lsp-dafny-counterexample-separator
              'display '(height (progn 1))))

(defconst lsp-dafny--counterexamples-empty-line
  (propertize "\n" 'display '(height (progn 1))))

(defun lsp-dafny--counterexamples-make-margin (width)
  "Prepare a propertized string to indent a counterexample inset by WIDTH."
  (concat (propertize " " 'display `(space :align-to ,width :height (1)))
          lsp-dafny--counterexamples-border))

(defun lsp-dafny--counterexamples-format-line (variable value indent)
  "Format a VARIABLE and its VALUE for display, indented by INDENT."
  (let* ((sep (string-match ":" variable))
         (typ (if sep (substring variable (1+ sep)) "??"))
         (variable (if sep (substring variable 0 sep) variable))
         (line (format "%s%s: %s := %s\n"
                       indent
                       (propertize variable 'face font-lock-variable-name-face)
                       (propertize typ 'face font-lock-type-face)
                       value)))
    line))

(defun lsp-dafny--counterexamples-format-inset (variables indent)
  "Format VARIABLES extracted from a counterexample for display.

Prefix each line with INDENT."
  (let* ((indent-amt (string-width indent))
         (indent-str (lsp-dafny--counterexamples-make-margin indent-amt))
         (lines (ht-map (lambda (var val)
                          (lsp-dafny--counterexamples-format-line
                           var val indent-str))
                        variables))
         (str (if lines
                   (string-join lines "")
                 lsp-dafny--counterexamples-empty-line)))
      (add-face-text-property 0 (length str)
                              'lsp-dafny-counterexample-inset t str)
      str))

(defun lsp-dafny--counterexamples-cleanup-1 (str)
  "Clean up Dafny values in STR (remove `_default' and `_module')."
  (replace-regexp-in-string "\\_<_\\(default\\|module\\)\\_>[.]" "" str))

(defun lsp-dafny--counterexamples-cleanup-variables (counterexamples)
  "Clean up variable types in COUNTEREXAMPLES."
  ;; FIXME: Move this to the server?
  (pcase-dolist ((dafny:counterexample :variables) counterexamples)
    (pcase-dolist (`(,var . ,val) (ht->alist variables))
      (let ((var_ (lsp-dafny--counterexamples-cleanup-1 var))
            (val_ (lsp-dafny--counterexamples-cleanup-1 val)))
        (unless (and (equal var var_) (equal val val_))
          (ht-remove variables var)
          (ht-set variables var_ val_)))))
  counterexamples)

(defun lsp-dafny--counterexamples-render-one (cx)
  "Create an overlay for one counter example CX and return it."
  (pcase-let* (((dafny:counterexample :position :variables) cx)
               (pt (lsp--position-to-point position)))
    (goto-char pt)
    (let* ((bol (progn (beginning-of-line) (point)))
           (eoi (progn (skip-chars-forward " \t") (point)))
           (eol (progn (end-of-line) (point)))
           (next-bol (min (1+ eol) (point-max)))
           (indent (buffer-substring bol eoi))
           (prefix (if (equal next-bol eol) "\n" ""))
           (separator lsp-dafny--counterexamples-separator)
           (text (lsp-dafny--counterexamples-format-inset variables indent)))
      (let ((ov (make-overlay next-bol next-bol nil nil nil)))
        (overlay-put ov 'after-string (concat prefix separator text))
        ov))))

(defun lsp-dafny--counterexamples-show ()
  "Show counterexamples in the current buffer."
  (lsp-dafny--counterexamples-hide)
  (lsp-dafny--with-whole-buffer-unmodified
    (let* ((params `(:textDocument ,(lsp--versioned-text-document-identifier)))
           (insets (lsp-request "dafny/counterExample" params))
           (inhibit-field-text-motion t)
           (inhibit-point-motion-hooks t))
      (pcase-dolist (cx (lsp-dafny--counterexamples-cleanup-variables insets))
        (push (lsp-dafny--counterexamples-render-one cx)
              lsp-dafny--counterexamples)))))

(define-minor-mode lsp-dafny-counterexamples-mode
  "Display verification counterexamples in the current buffer."
  :lighter nil
  (pcase lsp-dafny-counterexamples-mode
    (`t (lsp-dafny--counterexamples-show))
    (`nil (lsp-dafny--counterexamples-hide))))

(defun lsp-dafny-counterexamples-module (command)
  "Set up counterexample support for LSP Dafny according to COMMAND."
  (pcase command
    (`on)
    (`off (lsp-dafny--counterexamples-hide))))

;;;; Logging

;; (defcustom lsp-dafny-write-snapshots nil
;;   "Whether to write snapshots of the current file to disk when verifying it.")

;; (defun lsp-dafny--write-snapshot ()
;;   "Write a snapshot of the current buffer to disk."
;;   (let ((fname (format-time-string "%F-%H-%M-%S-%N.dfy")))
;;     (write-region nil nil fname)))

;; (defun lsp-dafny--maybe-write-snapshot ()
;;   "Write a snapshot to disk if `lsp-dafny-write-snapshots' is set."
;;   (when lsp-dafny-write-snapshots
;;     (lsp-dafny--write-snapshot)))

(defun lsp-dafny--log-io (message proc)
  "Log MESSAGE sent to PROC."
  (with-demoted-errors "Error in dafny--log-io: %S"
    (when (derived-mode-p 'dafny-mode)
      (let ((buf (format "*Dafny LSP IO: %s*" (process-id proc))))
        (with-current-buffer (get-buffer-create buf)
          (goto-char (point-max))
          (insert message))))))

(defun lsp-dafny-trace-module (command)
  "Set up Dafny LSP tracing according to COMMAND."
  (pcase command
   (`on
    (advice-add 'lsp--send-no-wait :after #'lsp-dafny--log-io))
   (`off
    (advice-remove 'lsp--send-no-wait #'lsp-dafny--log-io))))

;;; Entry point

(defconst lsp-dafny--notification-handlers
  (map-into
   '(("serverStarted" . ignore)
     ("dafnyLanguageServerVersionReceived" . ignore)
     ("dafny/ghost/diagnostics" . ignore)
     ("dafny/verification/status/gutter" . ignore)
     ("dafny/textDocument/symbolStatus" . ignore)
     ("dafny/compilation/status" . lsp-dafny--handle-/compilation/status))
   'hash-table))

(defun lsp-dafny-ensure-executable (executable)
  "Ensure that EXECUTABLE exists."
  (if (executable-find executable) executable
    (user-error "Binary not found: %s" executable)))

(defun lsp-dafny--server-command ()
  "Compute the command to run Dafny's LSP server."
  `(,(lsp-dafny-ensure-executable (lsp-dafny--server-installed-executable))
    ,(pcase lsp-dafny-server-automatic-verification-policy
       ((and policy (or `never `onchange `onsave))
        (format "--documents:verify=%S" policy))
       (other (user-error "Invalid value %S in \
`lsp-dafny-server-automatic-verification-policy'" other)))
    ,@(pcase lsp-dafny-server-verification-time-limit
       (`nil nil)
       ((and limit (pred integerp))
        (list (format "--verifier:timelimit=%d" limit)))
       (other (user-error "Invalid value %S in \
`lsp-dafny-server-verification-time-limit'" other)))
    ,@lsp-dafny-server-args))

;;;###autoload
(defun lsp-dafny-register ()
  "Register the Dafny LSP server with `lsp-mode'."
  (add-to-list 'lsp-language-id-configuration
               '(dafny-mode . "dafny"))
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection #'lsp-dafny--server-command)
    :activation-fn (lsp-activate-on "dafny")
    :server-id 'dafny
    :download-server-fn #'lsp-dafny--server-install
    :notification-handlers lsp-dafny--notification-handlers
    :after-open-fn #'lsp-dafny--mode)))

;;;###autoload
(with-eval-after-load 'lsp-mode
  (lsp-dafny-register))

(provide 'lsp-dafny)
;;; lsp-dafny.el ends here
