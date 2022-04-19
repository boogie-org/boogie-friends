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
(require 'boogie-friends)

;;;; Customization

(defgroup lsp-dafny nil
  "Options for dafny-mode's LSP support."
  :group 'lsp
  :prefix "lsp-dafny"
  :link '(url-link "https://github.com/dafny-lang/dafny/"))

;;;;; Installation options

(defconst lsp-dafny-latest-known-version "3.3.0")

(defun lsp-dafny--version-safe-p (vernum)
  "Check whether VERNUM is a safe value for `lsp-dafny-preferred-version'."
  (or (null vernum)
      (stringp vernum)))

(defcustom lsp-dafny-preferred-version nil
  "Which version of Dafny to run."
  :safe #'lsp-dafny--version-safe-p
  :type '(choice (const :tag "Auto-install the latest version" nil)
                 (choice :tag "Auto-install a specific version"
                         (const "3.0.0")
                         (const "3.1.0")
                         (const "3.2.0")
                         (const "3.3.0")
                         (const "3.4.0")
                         (string :tag "Other version"))
                 (const :tag "Find Dafny in your PATH" path)
                 (list :tag "Use a custom Dafny installation"
                       (directory :tag "Dafny installation directory"))))

(defcustom lsp-dafny-server-install-root
  (expand-file-name "dafny" lsp-server-install-dir)
  "Where to install Dafny servers."
  :risky t
  :type 'directory)

;;;;; Server options

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
           ((or `gnu `gnu/linux) "ubuntu-16.04")
           ((or `windows-nt `cygwin) "win")
           ((or `darwin) "osx-10.14.2")
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

;;;; Custom notifications

(lsp-interface (dafny:compilation/progress (:uri :status :message)))

(defvar-local lsp-dafny--verification-status nil)

(lsp-defun lsp-dafny--handle-/compilation/status
    (_workspace (&dafny:compilation/progress :uri :status :message))
  "Handle the dafny/compilation/status notification MSG."
  (with-current-buffer (-> uri lsp--uri-to-path find-file-noselect)
    (setq-local
     lsp-dafny--verification-status
     (pcase status
       ("ParsingFailed"
        '(failure "!" "Parsing error"))
       ("ResolutionFailed"
        '(failure "!" "Resolution error"))
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
        '(running ,other "Unknown status"))))
    (force-mode-line-update)))

(defconst lsp-dafny--notification-handlers
  (map-into
   '(("serverStarted" . ignore)
     ("dafnyLanguageServerVersionReceived" . ignore)
     ("dafny/ghost/diagnostics" . ignore)
     ("dafny/compilation/status" . lsp-dafny--handle-/compilation/status))
   'hash-table))

;;;; Overrides

(defun lsp-dafny-diagnostics--flycheck-start (checker callback)
  "Start an LSP syntax check with CHECKER.

CALLBACK is the status callback passed by Flycheck."

  (remove-hook 'lsp-on-idle-hook #'lsp-diagnostics--flycheck-buffer t)

  (->> (lsp--get-buffer-diagnostics)
       (-mapcat
        (-lambda ((&Diagnostic :message :severity? :tags? :code? :source? :related-information?
                               :range (&Range :start (&Position :line      start-line
                                                                :character start-character)
                                              :end   (&Position :line      end-line
                                                                :character end-character))))
          (let ((group (gensym)))
            (cons (flycheck-error-new
                   :buffer (current-buffer)
                   :checker checker
                   :filename buffer-file-name
                   :message message
                   :level (lsp-diagnostics--flycheck-calculate-level severity? tags?)
                   :id code?
                   :group group
                   :line (lsp-translate-line (1+ start-line))
                   :column (1+ (lsp-translate-column start-character))
                   :end-line (lsp-translate-line (1+ end-line))
                   :end-column (1+ (lsp-translate-column end-character)))
                  (-mapcat
                   (-lambda ((&DiagnosticRelatedInformation
                              :message
                              :location
                              (&Location :range (&Range :start (&Position :line      start-line
                                                                          :character start-character)
                                                        :end   (&Position :line      end-line
                                                                          :character end-character))
                                         :uri)))
                     (and (equal (-> uri lsp--uri-to-path lsp--fix-path-casing)
                                 (-> buffer-file-name lsp--fix-path-casing))
                          `(,(flycheck-error-new
                              :buffer (current-buffer)
                              :checker checker
                              :filename buffer-file-name
                              :message message
                              :level (lsp-diagnostics--flycheck-calculate-level (1+ severity?) tags?)
                              :id code?
                              :group group
                              :line (lsp-translate-line (1+ start-line))
                              :column (1+ (lsp-translate-column start-character))
                              :end-line (lsp-translate-line (1+ end-line))
                              :end-column (1+ (lsp-translate-column end-character))))))
                   related-information?)))))
       (funcall callback 'finished)))

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

;;;; Additional behaviors

(defun lsp-dafny--on-change (&rest _args)
  "Reset verification progress."
  (setq lsp-dafny--verification-status nil)
  (force-mode-line-update))

(defun lsp-dafny--mode-line-process-format ()
  "Compute text to display in Dafny's modeline."
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
                    ;;  '(concat "\t" icon "\t"))
                    icon)
                  'face face
                  'help-echo tooltip))))

(defconst lsp-dafny--mode-line-process
  '(:eval (lsp-dafny--mode-line-process-format)))
(put 'lsp-dafny--mode-line-process 'risky-local-variable t)

(define-minor-mode lsp-dafny--mode
  "Minor mode that implements Dafny features that depend on LSP."
  :init-value nil
  ;; :lighter (:eval (lsp-dafny--mode-line-text))
  (cond
   (lsp-dafny--mode
    (make-local-variable 'mode-line-process)
    (add-hook 'after-change-functions #'lsp-dafny--on-change nil t)
    (add-function :override
                  (local 'lsp-diagnostics--flycheck-start)
                  #'lsp-dafny-diagnostics--flycheck-start)
    (if mode-line-process
        (add-to-list 'mode-line-process 'lsp-dafny--mode-line-process t)
      (setq mode-line-process '("" lsp-dafny--mode-line-process)))
    (setq-local company-backends
                (cons #'company-capf (remove #'company-capf company-backends))))
   (t
    (remove-hook 'after-change-functions #'lsp-dafny--on-change t)
    (remove-function (local 'lsp-diagnostics--flycheck-start)
                     #'lsp-dafny-diagnostics--flycheck-start)
    (setq-local mode-line-process
                (delq 'lsp-dafny--mode-line-process mode-line-process))
    (setq-local company-backends
                (remove #'company-capf company-backends)))))

(define-minor-mode lsp-dafny-trace-mode
  "Minor mode that records all queries to the server."
  :init-value nil
  (cond
   (lsp-dafny-trace-mode
    (advice-add 'lsp--send-no-wait :after #'lsp-dafny--log-io))
   (t
    (advice-remove 'lsp--send-no-wait #'lsp-dafny--log-io))))

;;;; Entry point

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

(defun lsp-dafny--after-open-fn ()
  "Turn `dafny-mode' features that depend on LSP."
  (lsp-dafny--mode))

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
    :after-open-fn #'lsp-dafny--after-open-fn)))

;;;###autoload
(with-eval-after-load 'lsp-mode
  (lsp-dafny-register))

(provide 'lsp-dafny)
;;; lsp-dafny.el ends here
