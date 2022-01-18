;;; inferior-dafny.el --- Using Dafny as a verification server -*- lexical-binding: t -*-

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

;; See boogie-friends.el for general information about this package.
;;
;; This file offers of a more sophisticated method for verifying buffers on the
;; fly than the original one.  The checker defined in dafny-mode.el simply calls
;; the standard Dafny CLI for each change, reverifying the entire source file
;; every time.  This is inefficient, especially when editing a few functions in a
;; larger file.
;;
;; This file implements a client for the Dafny server, a command line utility
;; that allows non-.Net editors to take advantage of Dafny's caching facilities,
;; as used by the Dafny extension for Visual Studio.  The server is essentially a
;; REPL, which produces output in the same format as the Dafny CLI; clients thus
;; do not need to understand the internals of Dafny's caching.  A typical editing
;; session proceeds as follows:
;;
;; * When a new Dafny file is opened, the editor starts a new instance of the
;;   Dafny server.  The cache is blank at that point.
;; * The editor sends a copy of the buffer for initial verification.  This takes
;;   some time, after which the server returns a list of errors.
;; * The user makes modifications; the editor periodically sends a new copy of
;;   the buffer's contents to the Dafny server, which quickly returns an updated
;;   list of errors.
;;
;; The client-server protocol is sequential, uses JSON, and works over ASCII
;; pipes by base64-encoding utf-8 queries.  It defines one type of query, and two
;; types of responses:
;;
;; Queries are of the following form:
;;    verify
;;    <base64 encoded JSON payload>
;;    [[DAFNY-CLIENT: EOM]]
;;
;; Responses are of the following form:
;;    <list of errors and usual output, as produced by the Dafny CLI>
;;    [SUCCESS] [[DAFNY-SERVER: EOM]]
;; or
;;    <error message>
;;    [FAILURE] [[DAFNY-SERVER: EOM]]
;;
;; The JSON payload is an utf-8 encoded string resulting of the serialization of
;; a dictionary with 4 fields:
;;    * args:   An array of Dafny arguments, as passed to the Dafny CLI
;;    * source: A Dafny program, or the path to a Dafny source file.
;;    * sourceIsFile: A boolean indicating whether the 'source' argument is a
;;                    Dafny program or the path to one.
;;    * filename:     The name of the original source file, to be used in error
;;                    messages
;;
;; For small files, embedding the Dafny source directly into a message is
;; convenient; for larger files, however, it is generally better for performance
;; to write the source snapshot to a separate file, and to pass that to Dafny
;; by setting the 'sourceIsFile' flag to true.

;;; Code:

(require 'json)
(require 'flycheck)
(require 'boogie-friends)

;;; Customization

;; forward declarations (see dafny-mode.el)
(defvar dafny--flycheck-extra)
(defvar dafny-verification-backend)

(boogie-friends-def-exec dafny inferior-dafny "DafnyServer")
(defun flycheck-inferior-dafny-executable ()
  "Return `flycheck-inferior-dafny-executable' or a default value."
  (or flycheck-inferior-dafny-executable "DafnyServer"))

(defvar inferior-dafny--in-memory nil
  "If non-nil, pass the buffer contents as part of queries to the server.

When nil, the contents of the buffer are saved to a temporary
location, and the server is directed to that location.")

(defvar inferior-dafny--write-snapshots nil
  "If non-nil, save a copy of the buffer after every query.")

(defvar inferior-dafny--transcript-name "dafny-client.transcript"
  "A file name under which to save session transcripts.")

(defvar inferior-dafny--write-transcript nil
  "If non-nil, write queries to a session transcript.

The transcript file is saved under the name specified in variable
`inferior-dafny--transcript-name'.")

;; FIXME: Menu?

;;; Debugging functions

(defvar inferior-dafny--debug nil
  "When non-nil, print debug output.")

(defmacro inferior-dafny-debug (&rest args)
  "Call `message' on ARGS if `inferior-dafny--debug' is non-nil."
  `(if inferior-dafny--debug
       (message ,@args)))

(defmacro inferior-dafny-info (&rest args)
  "Call `message' on ARGS."
  `(message ,@args))

(defun inferior-dafny-toggle-debug ()
  "Toggle whether to show debug information."
  (interactive)
  (setq inferior-dafny--debug (not inferior-dafny--debug))
  (inferior-dafny-info "inferior-dafny-debug %s"
         (if inferior-dafny--debug "enabled" "disabled")))

;;; Status variables and utilities

(defconst inferior-dafny-client-eom-tag "[[DAFNY-CLIENT: EOM]]"
  "String that gets appended to each message sent to the server.")
(defconst inferior-dafny-server-eom-tag "[[DAFNY-SERVER: EOM]]"
  "String that marks the end of the server's responses.")
(defconst inferior-dafny-server-eom-tag-regexp
  (format "^%s %s$" "\\[\\(SUCCESS\\|FAILURE\\)\\]"
          (regexp-quote inferior-dafny-server-eom-tag))
  "Regexp matching the end tag of both prover end tags.")

(defconst inferior-dafny-status-regexp
  "\\`\\(CheckWellFormed\\|Impl\\)\\$\\$.+\\.\\([^.]+\\)\\.\\([^.]+\\)\\'"
  "Regexp to clean up Boogie method names.")

(defconst inferior-dafny-process-name-template
  " *inferior-dafny--%s*"
  "Format string to name the Dafny server process.")
(defconst inferior-dafny-process-buffer-name-template
  " *inferior-dafny-process--%s*"
  "Format string to name the Dafny server output buffer.")

(defun inferior-dafny-process-name ()
  "Get the name of the server process for the current buffer."
  (format inferior-dafny-process-name-template (buffer-name)))

(defun inferior-dafny-process-buffer-name ()
  "Get the name of the server buffer for the current buffer."
  (format inferior-dafny-process-buffer-name-template (buffer-name)))

(defun inferior-dafny-process-buffer ()
  "Get the buffer used to store the server's responses."
  (get-buffer (inferior-dafny-process-buffer-name)))

(defvar-local inferior-dafny--busy nil
  "Are we waiting for the server to respond?")
(defvar-local inferior-dafny--process nil
  "The Dafny server inferior process.")
(defvar-local inferior-dafny--callback nil
  "A function to call after verification completes.")

(defconst inferior-dafny--parent-buffer 'inferior-dafny--parent-buffer
  "Key to put parent buffer under in the server process' plist.")

;;; Initialization

(defun inferior-dafny-make-process-buffer ()
  "Create or recycle a buffer to store the server's responses."
  (let ((proc-buf (get-buffer-create (inferior-dafny-process-buffer-name))))
    (with-current-buffer proc-buf
      (add-hook 'kill-buffer-hook #'inferior-dafny-process-buffer-killed)
      (buffer-disable-undo)
      (erase-buffer))
    proc-buf))

(defun inferior-dafny-start-process ()
  "Start a Dafny server process.

If the server is already running, kill it first.  The
corresponding output buffer is created or recycled."
  (inferior-dafny-kill)
  ;; Setting `process-connection-type' to nil ensures that we use a pipe instead
  ;; of a TTY; TTYs leak the TERM variable, causing Mono to print syntax-highted
  ;; error messages, which confuse our parser (also, TTYs are horribly slow).
  (let* ((process-connection-type nil)
         (proc-name (inferior-dafny-process-name))
         (proc-buf (inferior-dafny-make-process-buffer))
         (proc (start-process proc-name proc-buf
			      (flycheck-inferior-dafny-executable))))
    (set-process-query-on-exit-flag proc nil)
    (set-process-coding-system proc 'utf-8 'utf-8)
    (set-process-filter proc #'inferior-dafny-filter)
    (set-process-sentinel proc #'inferior-dafny-sentinel)
    (process-put proc inferior-dafny--parent-buffer (current-buffer))
    proc))

(defun inferior-dafny-init ()
  "Launch the Dafny server."
  (interactive)
  (setq inferior-dafny--process (inferior-dafny-start-process))
  (add-hook 'kill-buffer-hook #'inferior-dafny-parent-buffer-killed)
  (inferior-dafny-info "Started inferior-dafny process for buffer %s"
         (buffer-name)))

(defun inferior-dafny-live-p ()
  "Check if the Dafny server is already running."
  (and inferior-dafny--process
       (process-live-p inferior-dafny--process)))

;;; Verification

(defun inferior-dafny-get-source-as-file ()
  "Save a snapshot of the buffer and return its path in a plist."
  (let ((temp-fname (flycheck-save-buffer-to-temp
                     #'flycheck-temp-file-system)))
    `(:source ,temp-fname :sourceIsFile t)))

(defun inferior-dafny-get-source-as-string ()
  "Make a snapshot of the buffer and return it in a plist."
  (save-restriction
    (widen)
    `(:source ,(buffer-string) :sourceIsFile nil)))

(defun inferior-dafny-get-source ()
  "Prepare the :source and :sourceIsFile part of a query."
  (if inferior-dafny--in-memory
      (inferior-dafny-get-source-as-string)
    (inferior-dafny-get-source-as-file)))

(defconst inferior-dafny-fname-placeholder "<unsaved>"
  "Value of :filename when the current file is unsaved.

The server doesn't recognize this value as special, but we do
when post-processing errors.")

(defun inferior-dafny-prepare-query ()
  "Prepare a query to the server."
  (let* ((json-false nil)
         (args  (boogie-friends-compute-prover-args))
         (fname (or buffer-file-name inferior-dafny-fname-placeholder))
         (src   (inferior-dafny-get-source))
         (json  (json-encode `(:args ,args :filename ,fname ,@src)))
         (json8 (encode-coding-string json 'utf-8 t))
         (b64   (concat (base64-encode-string json8)))
         (cmd   (format "%s\n%s\n%s\n" "verify" b64
                        inferior-dafny-client-eom-tag)))
    (inferior-dafny-debug "Sending [%s] [%s]" b64 cmd)
    cmd))

(defun inferior-dafny-write-snapshot ()
  "Write a snapshot of the current buffer to disk."
  (let ((fname (format-time-string "%F-%H-%M-%S-%N.dfy")))
    (write-region nil nil fname)))

(defun inferior-dafny-update-transcript ()
  "Log current query to file when appropriate."
  (when inferior-dafny--write-transcript
    (let* ((inferior-dafny--in-memory t)
           (query (inferior-dafny-prepare-query)))
      (append-to-file query nil inferior-dafny--transcript-name))))

(defun inferior-dafny-verify (_checker callback)
  "Issue a `verify' query to the server, returning immediately.

_CHECKER is ignored.  CALLBACK should be a function of two
arguments STATUS and DATA, as described in the docs of
`flycheck-report-buffer-checker-status'.  The verification
results are sent to CALLBACK when they become available.

Interactively, redirect verification results to a buffer.

If `inferior-dafny--busy' is non-nil, complain loudly."
  (interactive (list 'dafny-server #'inferior-dafny-debug-callback))
  (when inferior-dafny--busy
    (error "Dafny server already busy"))
  (unless (inferior-dafny-live-p)
    (inferior-dafny-init))
  (setq inferior-dafny--busy (current-time)
        inferior-dafny--callback callback
        dafny--flycheck-extra nil)
  (if (inferior-dafny-live-p)
      (progn
        (when inferior-dafny--write-snapshots
          (inferior-dafny-write-snapshot))
        (inferior-dafny-update-transcript)
        (process-send-string inferior-dafny--process (inferior-dafny-prepare-query)))
    (inferior-dafny-callback 'errored "Could not start server")))

(defmacro inferior-dafny-with-parent-buffer (proc-or-buf &rest body)
  "Find Dafny buffer that spawned PROC-OR-BUF and run BODY there."
  (declare (indent defun))
  `(-when-let* ((proc (if (bufferp ,proc-or-buf)
                          (get-buffer-process ,proc-or-buf)
                        ,proc-or-buf))
                (source-buf (process-get proc inferior-dafny--parent-buffer))
                (source-buf-live (buffer-live-p source-buf)))
     (with-current-buffer source-buf
       ,@body)))

(defun inferior-dafny-sentinel (proc signal)
  "Sentinel function for PROC handling SIGNAL."
  (inferior-dafny-debug "Got signal [%s]; process-live-p: [%s]"
          signal (process-live-p proc))
  (when (not (process-live-p proc))
    (inferior-dafny-with-parent-buffer proc
      (inferior-dafny-killed))))

(defun inferior-dafny-filter (proc string)
  "Filter function for the server process PROC.

STRING is the newly received output."
  (inferior-dafny-debug "Got output (length: [%d])" (length string))
  (if (buffer-live-p (process-buffer proc))
      (inferior-dafny-filter-live proc string)
    (inferior-dafny-filter-dead proc string)))

(defun inferior-dafny-filter-live (proc string)
  "Handle new output from server process PROC.

Insert STRING in the output buffer, and look for
`inferior-dafny-server-eom-tag'.  Call
`inferior-dafny-handle-full-response' if it is found."
  (inferior-dafny-debug "[inferior-dafny-filter-live] [%s]" string)
  (with-current-buffer (process-buffer proc)
    (goto-char (point-max))
    (insert string)
    (inferior-dafny-update-status (process-mark proc) (point))
    (set-marker (process-mark proc) (point))
    (forward-line 0) ;; end tag is always on its own line
    (-when-let* ((final-status (inferior-dafny-find-eom-tag))
                 (resp-beg     (point-min))
                 (resp-end     (point-at-bol))
                 (tag-end      (point-at-eol))
                 (response     (buffer-substring-no-properties
                                resp-beg resp-end)))
      (delete-region resp-beg tag-end)
      (inferior-dafny-handle-full-response response final-status proc))))

(defun inferior-dafny-filter-dead (proc string)
  "Handle unexpected output: kill PROC by printing STRING."
  (inferior-dafny-debug "[inferior-dafny-filter-dead] [%s]" string)
  ;; FIXME: should be able to trace to the original buffer
  (kill-process proc)
  (error "[inferior-dafny] Got unexpected output from the server: %s" string))

(defun inferior-dafny-find-eom-tag ()
  "Find the server's end tag in the current buffer.

First search for `inferior-dafny-server-eom-tag' (text searches
are faster), then read status using."
  (goto-char (point-min))
  (when (search-forward inferior-dafny-server-eom-tag nil t)
    (beginning-of-line)
    (or (and (looking-at inferior-dafny-server-eom-tag-regexp)
             (match-string-no-properties 1))
        "UNKNOWN STATUS")))

(defun inferior-dafny-handle-full-response (response status proc)
  "Handle a RESPONSE and STATUS from the server PROC.

Pass the corresponding error list (or the error message,
depending on STATUS) to the callback that was registered when
verification was initiated."
  (inferior-dafny-debug "[inferior-dafny-handle-full-response] [%s] [%s]"
          status response)
  (inferior-dafny-with-parent-buffer proc
    (cond ((equal status "SUCCESS")
           (inferior-dafny-callback
            'finished (inferior-dafny-parse-errors response)))
          (t
           (inferior-dafny-callback
            'errored  response)))))

(defun inferior-dafny-update-status (beg end)
  "Look for and display a status message.

The status message in searched for in the region that spans from
the beginning of the line on which BEG is found to END."
  (save-excursion
    (setq dafny--flycheck-extra nil)
    (let ((bound (progn (goto-char beg)
                        (beginning-of-line)
                        (point))))
      (goto-char end)
      (when (and (boundp 'boogie-friends-trace-header-regexp)
                 (re-search-backward boogie-friends-trace-header-regexp bound t))
        (-when-let* ((name    (match-string-no-properties 1))
                     (clean   (replace-regexp-in-string
                               inferior-dafny-status-regexp "\\3" name))
                     (wrapped (format "[%s]" clean)))
          (setq dafny--flycheck-extra wrapped)))))
  (inferior-dafny-with-parent-buffer (current-buffer)
    (force-mode-line-update)))

(defun inferior-dafny-reset-file-names (errors)
  "Adjust file names in ERRORS.

Replace each instance of `inferior-dafny-fname-placeholder' with
the value of variable `buffer-file-name'."
  ;; FIXME : Should maybe reset all names,
  ;; since Boogie seems to always return the cached name
  (mapc (lambda (err)
          (when (equal (flycheck-error-filename err)
                       inferior-dafny-fname-placeholder)
            (setf (flycheck-error-filename err) buffer-file-name)))
        errors))

(defun inferior-dafny-parse-errors (response)
  "Parse RESPONSE, extracting error messages."
  (boogie-friends-cleanup-errors
   (inferior-dafny-reset-file-names
    (flycheck-increment-error-columns
     (flycheck-parse-with-patterns response 'dafny (current-buffer))))))

(defun inferior-dafny-callback (status &optional data)
  "Forward STATUS and DATA to the currently registered callback.

This function is called after verification completes; it resets
`inferior-dafny--busy' and `inferior-dafny--callback' in
preparation for the next verification."
  (let ((callback inferior-dafny--callback)
        (was-busy inferior-dafny--busy))
    (setq inferior-dafny--busy nil
          inferior-dafny--callback nil
          dafny--flycheck-extra nil)
    (unless (eq (null was-busy) (null callback))
      (error "Got unexpected status: [%s] [%s]" was-busy callback))
    (when (and was-busy callback)
      (inferior-dafny-debug "Verification took %.2fs"
              (float-time (time-since was-busy)))
      ;; Careful: The callback may launch another verification.
      ;; We don't want to prevent it, so --busy and --callback must be nil here
      (funcall callback status data))))

(defun inferior-dafny-debug-callback (status data)
  "Show STATUS and DATA in a debugging buffer."
  (with-current-buffer (get-buffer-create "*inferior-dafny-output*")
    (erase-buffer)
    (insert (format "%s\n" (current-time-string)))
    (insert (format "Status: %s\n" status))
    (insert (format "Output:\n%s\n" data))
    (display-buffer (current-buffer))))

;;; Termination

(defun inferior-dafny-reset ()
  "Kill all servers and their output buffers, and reset flycheck.

Mostly useful for debugging, or when the client seems to be
confused about the prover's state."
  (interactive)
  (cl-loop for b being the buffers
           do (when (and (buffer-live-p b)
                         (eq (buffer-local-value 'major-mode b)
                             'dafny-mode))
                (with-current-buffer b
                  (inferior-dafny-kill t)
                  (flycheck-teardown)))))

(defun inferior-dafny-kill (&optional kill-buffer)
  "Kill the server.

If KILL-BUFFER is non-nil, get rid of its output buffer as well."
  (interactive '(t))
  (when (inferior-dafny-live-p)
    (inferior-dafny-debug "Killing existing inferior dafny process")
    (kill-process inferior-dafny--process))
  (when kill-buffer
    (-when-let* ((buf (inferior-dafny-process-buffer)))
      (kill-buffer buf)))
  (inferior-dafny-killed))

(defun inferior-dafny-killed ()
  "Reset various variables after the server is killed."
  (when inferior-dafny--callback
    (inferior-dafny-callback 'interrupted "Killed"))
  (setq inferior-dafny--busy nil
        inferior-dafny--callback nil
        inferior-dafny--process nil
        dafny--flycheck-extra nil))

(defun inferior-dafny-parent-buffer-killed ()
  "Value for `kill-buffer-hook' in the source buffer."
  (inferior-dafny-kill t))

(defun inferior-dafny-process-buffer-killed ()
  "Value for `kill-buffer-hook' in the output buffer."
  (inferior-dafny-with-parent-buffer (current-buffer)
    ;; nil because (re-)Closing the buffer in
    ;; inferior-dafny-kill would cause a loop
    (inferior-dafny-kill nil)))

;;; Health-checking

(defun inferior-dafny-healthcheck-selftest ()
  "Send a self-test query to the server and check the output."
  (-when-let* ((output (ignore-errors
                         (process-lines
                          (flycheck-inferior-dafny-executable) "-selftest")))
               (status-line (car-safe (last output))))
    (save-match-data
      (and (string-match inferior-dafny-server-eom-tag-regexp status-line)
           (equal (match-string 1 status-line) "SUCCESS")))))

(defun inferior-dafny-healthcheck-ok-status ()
  "Prepare a message for `inferior-dafny-healthcheck'."
  (concat "ok " (if (inferior-dafny-live-p) "(running)" "(not started yet)")))

(defun inferior-dafny-healthcheck-message (header status t-message nil-message)
  "Wrap a message in a `flycheck-verification-result'.

HEADER is the name of test, Depending on STATUS, it is followed
by T-MESSAGE or NIL-MESSAGE."
  (list (flycheck-verification-result-new
         :label header
         :message (if status t-message nil-message)
         :face (if status 'success '(bold error)))))

(defun inferior-dafny-healthcheck (&optional _checker)
  "Produce a health report for flycheck's :verify property."
  (let* ((exec (and (flycheck-inferior-dafny-executable)
                    (executable-find (flycheck-inferior-dafny-executable))))
         (test (and exec (inferior-dafny-healthcheck-selftest))))
    (append
     (inferior-dafny-healthcheck-message
      "executable" exec (concat "found at " exec) "not found")
     (inferior-dafny-healthcheck-message
      "self-test" test (inferior-dafny-healthcheck-ok-status) "failed"))))

;;; Flycheck setup

(defun inferior-dafny-interrupt (&rest _args)
  "Call `inferior-dafny-kill', ignoring _ARGS."
  (inferior-dafny-kill))

(defun inferior-dafny-predicate ()
  "Predicate for `flycheck-define-generic-checker'."
  (or (eq dafny-verification-backend 'server)
      (bound-and-true-p boogie-friends--prover-running-in-foreground-p)))

(flycheck-define-generic-checker 'dafny-server
  "A Dafny checker using a background caching server process."
  ;; FIXME interrupt could send C-c to the prover to
  ;; stop the current check without loosing the cache
  :start #'inferior-dafny-verify
  :interrupt #'inferior-dafny-interrupt
  :verify #'inferior-dafny-healthcheck
  :predicate #'inferior-dafny-predicate
  :modes '(dafny-mode))

(add-to-list 'flycheck-checkers 'dafny-server)

(provide 'inferior-dafny)
;;; inferior-dafny.el ends here
