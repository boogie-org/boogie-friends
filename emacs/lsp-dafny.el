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

(require 'boogie-friends)
(require 'lsp nil t)

(defgroup lsp-dafny nil
  "Options for dafny-mode's LSP support."
  :group 'lsp
  :prefix "lsp-dafny")

(defcustom lsp-dafny-dotnet-executable "dotnet"
  "Dotnet executable used to run the Dafny server."
  :group 'lsp-dafny
  :risky t
  :type 'file)

(defcustom lsp-dafny-dll nil
  "Dafny language server executable."
  :group 'lsp-dafny
  :risky t
  :type 'file)

(defcustom lsp-dafny-args '("--documents:verify=onchange"
                            "--verifier:timelimit=0")
  ;; FIXME "--ghost:markStatements=true"
  "Dafny language server arguments."
  :group 'lsp-dafny
  :risky t
  :type '(repeat string))

;;;###autoload
(defun lsp-dafny-register ()
  "Register the Dafny LSP server with `lsp-mode'."
  (add-to-list 'lsp-language-id-configuration
               '(dafny-mode . "dafny"))
  (lsp-register-client
   (make-lsp-client
    :new-connection
    (lsp-stdio-connection
     (lambda () `(,lsp-dafny-dll
                  ,@lsp-dafny-args))
     (lambda () (and (lsp-server-present? (list lsp-dafny-dotnet-executable))
                     (or (and lsp-dafny-dll
                              (file-name-absolute-p lsp-dafny-dll)
                              (file-exists-p lsp-dafny-dll))
                         (ignore
                          (lsp-log "`lsp-dafny-dll' unset or not found"))))))
    :activation-fn (lsp-activate-on "dafny")
    :server-id 'dafny)))

(provide 'lsp-dafny)
;;; lsp-dafny.el ends here
