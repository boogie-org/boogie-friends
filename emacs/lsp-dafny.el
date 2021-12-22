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

(defcustom lsp-dafny-preferred-version "3.3.0"
  "Which version of Dafny to run, when installing it automatically."
  :safe t
  :type '(choice (const :tag "Any version (download the latest)" nil)
                 (string :tag "A specific version")))

(defcustom lsp-dafny-server-install-root
  (expand-file-name "dafny" lsp-server-install-dir)
  "Where to install Dafny servers."
  :risky t
  :type 'directory)

(defcustom lsp-dafny-server-executable nil
  "Name of or path to the Dafny language server binary."
  :risky t
  :type '(choice (const :tag "Install Dafny automatically" nil)
                 (file :tag "Custom path")))

;;;;; Server options

(defcustom lsp-dafny-server-automatic-verification-policy 'onchange
  "When to verify Dafny code."
  :type '(choice (const :tag "Never trigger verification automatically" never)
                 (const :tag "Verify on change" onchange)
                 (const :tag "Verify on save" onsave)))

(defcustom lsp-dafny-server-verification-time-limit 0
  "How long to search for a proof before giving up."
  :type '(choice (const :tag "No limit" 0)
                 (integer :tag "Give up after this many seconds")))

;; TODO "--ghost:markStatements=true"

(defcustom lsp-dafny-server-args nil
  "Dafny language server arguments."
  :risky t
  :type '(repeat string))

;;; LSP setup

;;;; Server installation

(defun lsp-dafny-server-install-dir (&optional vernum)
  "Compute the path to Dafny's installation folder for version VERNUM.

VERNUM defaults to `lsp-dafny-preferred-version'."
  (setq vernum (or vernum lsp-dafny-preferred-version))
  (expand-file-name (format "v%s" vernum) lsp-dafny-server-install-root))

(defun lsp-dafny--installed-executable (executable)
  "Compute the path to an installed Dafny EXECUTABLE."
  (expand-file-name executable (lsp-dafny-server-install-dir)))

(defun lsp-dafny--server-installed-executable ()
  "Compute the path to the installed version of DafnyLanguageServer."
  (lsp-dafny--installed-executable "DafnyLanguageServer"))

(defun lsp-dafny--zip-url (&optional vernum)
  "Compute the URL to download Dafny version VERNUM.

VERNUM defaults to `lsp-dafny-preferred-version'."
  (let ((platform
         (pcase system-type
           ((or `gnu `gnu/linux) "ubuntu-16.04")
           ((or `windows-nt `cygwin) "win")
           ((or `darwin) "osx-10.14.2")
           (other "Unsupported platform %S" other))))
    (setq vernum (or vernum lsp-dafny-preferred-version))
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
         (vernum lsp-dafny-preferred-version)
         (dl-dir (lsp-dafny-server-install-dir)))
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
        (list nil "!" "Parsing error"))
       ("ResolutionFailed"
        (list nil "!" "Resolution error"))
       ("CompilationSucceeded"
        (list 'running ""))
       ((and "VerificationInProgress" (guard message))
        (list 'running (format "[%s…]" message) "Verification in progress"))
       ((or "VerificationStarted" "VerificationInProgress")
        (list 'running "…" "Verification started"))
       ("VerificationFailed"
        (list nil "✗" "Verification failed"))
       ((or "Verification Succeeded")
        (list t "✓" "Verification succeeded"))))
    (force-mode-line-update)))

(defconst lsp-dafny--notification-handlers
  (map-into
   '(("serverStarted" . ignore)
     ("dafnyLanguageServerVersionReceived" . ignore)
     ("dafny/compilation/status" . lsp-dafny--handle-/compilation/status))
   'hash-table))

;;;; Additional info

(defun lsp-dafny--on-change (&rest _args)
  "Reset verification progress."
  (setq lsp-dafny--verification-status nil)
  (force-mode-line-update))

(defun lsp-dafny--mode-line-process-format ()
  "Compute text to display in Dafny's modeline."
  (pcase lsp-dafny--verification-status
    (`nil "")
    (`(,status ,icon ,tooltip)
     (propertize icon
                 'face `(bold ,(pcase status
                                 (`running 'compilation-mode-line-run)
                                 (`t 'compilation-mode-line-exit)
                                 (`nil 'compilation-mode-line-fail)))
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
    (if mode-line-process
        (add-to-list 'mode-line-process 'lsp-dafny--mode-line-process t)
      (setq mode-line-process `("" lsp-dafny--mode-line-process))))
   (t
    (remove-hook 'after-change-functions #'lsp-dafny--on-change t)
    (setq-local mode-line-process
                (delq 'lsp-dafny--mode-line-process mode-line-process)))))

;;;; Entry point

(defun lsp-dafny--server-command ()
  "Compute the command to run Dafny's LSP server."
  `(,(pcase lsp-dafny-server-executable
       (`nil (lsp-dafny--server-installed-executable))
       ((and custom (pred executable-find)) custom)
       (other (user-error "Executable not found: %S.
Please customize `lsp-dafny-server-executable'" other)))
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
;; (with-eval-after-load 'lsp-mode
;;   (lsp-dafny-register))

(provide 'lsp-dafny)
;;; lsp-dafny.el ends here
