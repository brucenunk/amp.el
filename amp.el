;;; amp.el --- Amp IDE integration for Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Keegan Carruthers-Smith

;; Author: Keegan Carruthers-Smith <keegan.csmith@gmail.com>
;; Author: Amp <amp@ampcode.com>
;; Version: 0.0.3
;; Package-Requires: ((emacs "28.1") (websocket "1.12"))
;; Keywords: amp, ai, agent, assistant
;; URL: https://github.com/keegancsmith/amp.el

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:

;; This package provides integration between Emacs and the Amp CLI.
;; It allows Amp to see the file you currently have open, your cursor
;; position, your text selection, and diagnostics.
;;
;; Based on the amp.nvim plugin architecture.
;;
;; Multi-project support:
;; - One Amp server per project root (detected via project.el)
;; - Non-project buffers are supported via a special nil project
;; - Use `amp-start' to start a server for the current project
;; - Use `amp-stop' to stop the server for the current project
;; - Use `amp-stop-all' to stop all servers
;; - Use `amp-status' to check status (with prefix arg for all servers)

;;; Code:

(require 'json)
(require 'websocket)
(require 'cl-lib)
(require 'project)

;;; Configuration

(defgroup amp nil
  "Amp IDE integration for Emacs."
  :group 'tools
  :prefix "amp-")

(defcustom amp-log-level 'info
  "Logging level for Amp plugin.
One of: trace, debug, info, warn, error."
  :type '(choice (const :tag "Trace" trace)
                 (const :tag "Debug" debug)
                 (const :tag "Info" info)
                 (const :tag "Warn" warn)
                 (const :tag "Error" error))
  :group 'amp)

;;; Project State Structure

(cl-defstruct amp--project-state
  root
  server
  port
  auth-token
  clients
  connected
  latest-selection
  latest-visible-files)

;;; State

(defvar amp--projects (make-hash-table :test #'equal)
  "Map project root (string or nil) to `amp--project-state'.")

(defvar amp--selection-timer nil
  "Timer for debounced selection updates.")

(defvar amp--event-listeners (make-hash-table :test 'equal)
  "Event listeners by event name.")

(defvar amp--hooks-installed nil
  "Whether global tracking hooks are installed.")

(defvar amp--cleanup-hook-installed nil
  "Whether the kill-emacs cleanup hook is installed.")

;;; Logging

(defvar amp--log-buffer "*amp-log*"
  "Buffer name for Amp log messages.")

(defvar amp--last-error nil
  "Most recent error (cons of timestamp, context, and message).")

(defun amp--log (level context message &rest args)
  "Log a message at LEVEL from CONTEXT with MESSAGE and ARGS."
  (let* ((levels '((trace . 0) (debug . 1) (info . 2) (warn . 3) (error . 4)))
         (current-level (or (cdr (assq amp-log-level levels)) 2))
         (msg-level (or (cdr (assq level levels)) 2)))
    (when (>= msg-level current-level)
      (let ((formatted-msg (apply #'format message args)))
        (with-current-buffer (get-buffer-create amp--log-buffer)
          (goto-char (point-max))
          (insert (format "[%s] [%s] %s: %s\n"
                          (format-time-string "%H:%M:%S")
                          (upcase (symbol-name level))
                          context
                          formatted-msg)))
        (when (eq level 'error)
          (setq amp--last-error (list (float-time) context formatted-msg)))
        (when (memq level '(warn error))
          (message "[Amp %s] %s" (upcase (symbol-name level)) formatted-msg))))))

;;; Project Detection

(defun amp--current-project-root ()
  "Return the normalized project root for the current buffer, or nil.
Returns nil for non-project buffers, which is a valid project key."
  (when-let* ((proj (project-current nil))
              (root (project-root proj)))
    (expand-file-name root)))

(defun amp--describe-project (root)
  "Return a human-readable description for ROOT (which may be nil)."
  (or (and root (abbreviate-file-name root))
      "non-project buffers"))

;;; Project Registry

(defun amp--get-project-state (root)
  "Get project state for ROOT, or nil if not running."
  (gethash root amp--projects))

(defun amp--put-project-state (state)
  "Register STATE in the project registry."
  (puthash (amp--project-state-root state) state amp--projects))

(defun amp--remove-project-state (root)
  "Remove project state for ROOT from registry."
  (remhash root amp--projects))

(defun amp--map-projects (fn)
  "Call FN with each project state in the registry."
  (maphash (lambda (_root state) (funcall fn state)) amp--projects))

;;; Utility Functions

(defun amp--get-data-home ()
  "Get the data directory for Amp, following the same logic as amp.nvim."
  (or (getenv "AMP_DATA_HOME")
      (let ((system-type-name (symbol-name system-type)))
        (cond
         ((string-match "windows" system-type-name)
          (expand-file-name "~/.local/share"))
         ((string-match "darwin" system-type-name)
          (expand-file-name "~/.local/share"))
         (t
          (or (getenv "XDG_DATA_HOME")
              (expand-file-name "~/.local/share")))))))

(defun amp--lock-dir ()
  "Get the lock directory path."
  (expand-file-name "amp/ide" (amp--get-data-home)))

(defun amp--generate-auth-token ()
  "Generate a random authentication token."
  (let ((chars "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")
        (token ""))
    (dotimes (_ 32)
      (setq token (concat token (string (aref chars (random (length chars)))))))
    token))

(defun amp--create-lockfile (port auth-token root)
  "Create a lockfile for PORT with AUTH-TOKEN and project ROOT.
ROOT may be nil for non-project buffers, in which case default-directory is used."
  (let* ((lock-dir (amp--lock-dir))
         (lockfile-path (expand-file-name (format "%d.json" port) lock-dir))
         (workspace (file-name-as-directory
                     (expand-file-name (or root default-directory))))
         (lock-data (json-encode
                     `((port . ,port)
                       (authToken . ,auth-token)
                       (pid . ,(emacs-pid))
                       (workspaceFolders . [,workspace])
                       (ideName . ,(format "Emacs %s" emacs-version))))))
    (unless (file-directory-p lock-dir)
      (make-directory lock-dir t))
    (with-temp-file lockfile-path
      (insert lock-data))
    lockfile-path))

(defun amp--remove-lockfile (port)
  "Remove the lockfile for PORT."
  (when port
    (let ((lockfile-path (expand-file-name (format "%d.json" port) (amp--lock-dir))))
      (when (file-exists-p lockfile-path)
        (delete-file lockfile-path)))))

;;; Event System

(defun amp--on (event-name callback)
  "Register CALLBACK for EVENT-NAME."
  (let ((listeners (gethash event-name amp--event-listeners)))
    (puthash event-name (cons callback listeners) amp--event-listeners)))

(defun amp--emit (event-name &rest args)
  "Emit EVENT-NAME with ARGS to all registered listeners."
  (let ((listeners (gethash event-name amp--event-listeners)))
    (dolist (callback listeners)
      (apply callback args))))

;;; IDE Protocol

(defun amp--wrap-notification (notification)
  "Wrap NOTIFICATION in IDE protocol format."
  `((serverNotification . ,notification)))

(defun amp--wrap-response (id response)
  "Wrap RESPONSE with ID in IDE protocol format."
  `((serverResponse . ,(cons `(id . ,id) response))))

(defun amp--wrap-error (id error)
  "Wrap ERROR with ID in IDE protocol format."
  `((serverResponse . ((id . ,id) (error . ,error)))))

;;; WebSocket Server

(defun amp--handle-websocket-message (state ws frame)
  "Handle incoming WebSocket message for project STATE from WS with FRAME."
  (let* ((message (websocket-frame-text frame))
         (parsed (condition-case err
                     (json-read-from-string message)
                   (error
                    (amp--log 'warn "server" "Invalid JSON: %s" message)
                    nil)))
         (request (and parsed (alist-get 'clientRequest parsed))))
    (when request
      (amp--handle-ide-request state ws request))))

(defun amp--handle-ide-request (state ws request)
  "Handle IDE REQUEST for project STATE from websocket WS."
  (let ((id (alist-get 'id request)))
    (cond
     ;; Ping request
     ((alist-get 'ping request)
      (amp--send-ide ws (amp--wrap-response id `((ping . ((message . ,(alist-get 'message (alist-get 'ping request)))))))))

     ;; Authenticate request
     ((alist-get 'authenticate request)
      (amp--send-ide ws (amp--wrap-response id '((authenticate . ((authenticated . t)))))))

     ;; Read file request
     ((alist-get 'readFile request)
      (let* ((read-req (alist-get 'readFile request))
             (path (alist-get 'path read-req)))
        (if (not path)
            (amp--send-ide ws (amp--wrap-error id '((code . -32602) (message . "Invalid params") (data . "readFile requires path parameter"))))
          (condition-case err
              (let ((content (with-temp-buffer
                               (insert-file-contents path)
                               (buffer-string))))
                (amp--send-ide ws (amp--wrap-response id `((readFile . ((success . t) (content . ,content) (encoding . "utf-8")))))))
            (error
             (amp--send-ide ws (amp--wrap-response id `((readFile . ((success . :json-false) (message . ,(error-message-string err))))))))))))

     ;; Edit file request
     ((alist-get 'editFile request)
      (let* ((edit-req (alist-get 'editFile request))
             (path (alist-get 'path edit-req))
             (full-content (alist-get 'fullContent edit-req)))
        (if (not path)
            (amp--send-ide ws (amp--wrap-error id '((code . -32602) (message . "Invalid params") (data . "editFile requires path parameter"))))
          (if (not full-content)
              (amp--send-ide ws (amp--wrap-error id '((code . -32602) (message . "Invalid params") (data . "editFile requires fullContent parameter"))))
            (condition-case err
                (let* ((full-path (expand-file-name path))
                       (bufnr (or (find-buffer-visiting full-path)
                                  (find-file-noselect full-path))))
                  (with-current-buffer bufnr
                    (let ((saved-point (point))
                          (saved-window-start (and (get-buffer-window bufnr)
                                                   (window-start (get-buffer-window bufnr)))))
                      (erase-buffer)
                      (insert full-content)
                      (goto-char (min saved-point (point-max)))
                      (when saved-window-start
                        (set-window-start (get-buffer-window bufnr)
                                          (min saved-window-start (point-max))))
                      (save-buffer)))
                  (amp--send-ide ws (amp--wrap-response id `((editFile . ((success . t) (message . ,(format "Edit applied successfully to %s" path)) (appliedChanges . t)))))))
              (error
               (amp--send-ide ws (amp--wrap-response id `((editFile . ((success . :json-false) (message . ,(error-message-string err)))))))))))))

     ;; Get diagnostics request
     ((alist-get 'getDiagnostics request)
      (let* ((diag-req (alist-get 'getDiagnostics request))
             (path (alist-get 'path diag-req)))
        (if (not path)
            (amp--send-ide ws (amp--wrap-error id '((code . -32602) (message . "Invalid params") (data . "getDiagnostics requires path parameter"))))
          (let ((entries (amp--get-diagnostics path)))
            (amp--send-ide ws (amp--wrap-response id `((getDiagnostics . ((entries . ,entries))))))))))

     ;; Unknown request
     (t
      (amp--send-ide ws (amp--wrap-error id '((code . -32601) (message . "Method not found") (data . "Unknown IDE request method"))))))))

(defun amp--send-ide (ws data)
  "Send IDE protocol DATA to websocket WS."
  (when (websocket-openp ws)
    (let ((json-message (json-encode data)))
      (condition-case err
          (websocket-send-text ws json-message)
        (error
         (amp--log 'warn "server" "Failed to send message: %s" (error-message-string err)))))))

(defun amp--project-broadcast-ide (state notification)
  "Broadcast NOTIFICATION to all clients of project STATE."
  (let ((json-message (json-encode notification)))
    (dolist (ws (amp--project-state-clients state))
      (when (websocket-openp ws)
        (condition-case err
            (websocket-send-text ws json-message)
          (error
           (amp--log 'warn "server" "Failed to broadcast: %s"
                     (error-message-string err))))))))

(defun amp--broadcast-ide (payload)
  "Broadcast PAYLOAD to the current project's server.
Resolves the project from the current buffer."
  (let* ((root (amp--current-project-root))
         (state (amp--get-project-state root)))
    (unless state
      (user-error "No Amp server running for %s" (amp--describe-project root)))
    (amp--project-broadcast-ide state (amp--wrap-notification payload))))

(defun amp--on-client-connect (state ws)
  "Handle client connection for project STATE from WS."
  (push ws (amp--project-state-clients state))
  (setf (amp--project-state-connected state) t)
  (amp--log 'info "server" "Client connected to project %s"
            (amp--describe-project (amp--project-state-root state)))
  (amp--emit 'client-connect ws)
  (run-with-timer 0.05 nil
                  (lambda ()
                    (amp--send-initial-state-for-project state))))

(defun amp--send-initial-state-for-project (state)
  "Send initial visible-files and selection state for project STATE."
  (amp--broadcast-visible-files-for-project state t)
  (amp--update-selection-for-project state))

(defun amp--on-client-disconnect (state ws)
  "Handle client disconnection for project STATE from WS."
  (setf (amp--project-state-clients state)
        (delq ws (amp--project-state-clients state)))
  (when (null (amp--project-state-clients state))
    (setf (amp--project-state-connected state) nil)
    (amp--log 'info "server" "Disconnected from Amp for project %s"
              (amp--describe-project (amp--project-state-root state))))
  (amp--emit 'client-disconnect ws))

;;; Selection Tracking

(defun amp--get-file-uri ()
  "Get the file URI for the current buffer."
  (when buffer-file-name
    (concat "file://" (expand-file-name buffer-file-name))))

(defun amp--get-cursor-selection ()
  "Get the current cursor position as a selection."
  (let ((uri (amp--get-file-uri)))
    (when uri
      (let* ((pos (point))
             (line (1- (line-number-at-pos pos)))
             (col (- pos (line-beginning-position)))
             (line-content (buffer-substring-no-properties
                            (line-beginning-position)
                            (line-end-position))))
        `((text . "")
          (fileUrl . ,uri)
          (selection . ((start . ((line . ,line) (character . ,col)))
                        (end . ((line . ,line) (character . ,col)))))
          (lineContent . ,line-content))))))

(defun amp--get-visual-selection ()
  "Get the current visual selection."
  (when (and (use-region-p) (amp--get-file-uri))
    (let* ((uri (amp--get-file-uri))
           (start-pos (region-beginning))
           (end-pos (region-end))
           (start-line (1- (line-number-at-pos start-pos)))
           (start-col (- start-pos (save-excursion (goto-char start-pos) (line-beginning-position))))
           (end-line (1- (line-number-at-pos end-pos)))
           (end-col (- end-pos (save-excursion (goto-char end-pos) (line-beginning-position))))
           (text (buffer-substring-no-properties start-pos end-pos)))
      `((text . ,text)
        (fileUrl . ,uri)
        (selection . ((start . ((line . ,start-line) (character . ,start-col)))
                      (end . ((line . ,end-line) (character . ,end-col)))))))))

(defun amp--get-current-selection ()
  "Get the current selection (visual or cursor)."
  (or (amp--get-visual-selection)
      (amp--get-cursor-selection)))

(defun amp--selection-to-ide-format (selection)
  "Convert SELECTION to IDE protocol format."
  (let ((sel (alist-get 'selection selection)))
    `((uri . ,(alist-get 'fileUrl selection))
      (selections . [((range . ((startLine . ,(alist-get 'line (alist-get 'start sel)))
                                (startCharacter . ,(alist-get 'character (alist-get 'start sel)))
                                (endLine . ,(alist-get 'line (alist-get 'end sel)))
                                (endCharacter . ,(alist-get 'character (alist-get 'end sel)))))
                      (content . ,(alist-get 'text selection)))]))))

(defun amp--update-selection ()
  "Update and broadcast selection for the current buffer's project.
Works for both project buffers and non-project buffers (nil project)."
  (let* ((root (amp--current-project-root))
         (state (amp--get-project-state root))
         (selection (amp--get-current-selection)))
    (when (and state selection)
      (let ((last (amp--project-state-latest-selection state)))
        (unless (equal selection last)
          (setf (amp--project-state-latest-selection state) selection)
          (let ((ide-notification (amp--selection-to-ide-format selection)))
            (amp--project-broadcast-ide
             state
             (amp--wrap-notification `((selectionDidChange . ,ide-notification))))
            (amp--log 'debug "selection" "Selection changed for %s"
                      (amp--describe-project root))))))))

(defun amp--update-selection-for-project (state)
  "Update and broadcast current selection for project STATE."
  (let* ((root (amp--project-state-root state))
         (selection nil))
    (cl-block find-selection
      (dolist (window (window-list))
        (let* ((buf (window-buffer window))
               (buf-root (with-current-buffer buf (amp--current-project-root))))
          (when (equal buf-root root)
            (with-current-buffer buf
              (setq selection (amp--get-current-selection)))
            (cl-return-from find-selection)))))
    (when selection
      (setf (amp--project-state-latest-selection state) selection)
      (let ((ide-notification (amp--selection-to-ide-format selection)))
        (amp--project-broadcast-ide
         state
         (amp--wrap-notification `((selectionDidChange . ,ide-notification))))))))

(defun amp--debounced-selection-update ()
  "Debounced selection update."
  (when amp--selection-timer
    (cancel-timer amp--selection-timer))
  (setq amp--selection-timer
        (run-with-timer 0.1 nil #'amp--update-selection)))

;;; Visible Files Tracking

(defun amp--get-visible-files-for-project (root)
  "Return list of file:// URIs for visible files in project ROOT.
ROOT may be nil for non-project files."
  (let ((seen (make-hash-table :test 'equal))
        (uris nil))
    (dolist (frame (frame-list))
      (dolist (window (window-list frame 'no-minibuf))
        (let* ((buf (window-buffer window))
               (file (buffer-file-name buf)))
          (when file
            (let ((buf-root (with-current-buffer buf
                              (amp--current-project-root))))
              (when (equal buf-root root)
                (let ((abs (expand-file-name file)))
                  (unless (gethash abs seen)
                    (puthash abs t seen)
                    (push (concat "file://" abs) uris)))))))))
    (nreverse uris)))

(defun amp--broadcast-visible-files-for-project (state &optional force)
  "Broadcast visible files for project STATE if changed or FORCE is non-nil."
  (let* ((root (amp--project-state-root state))
         (files (amp--get-visible-files-for-project root))
         (last (amp--project-state-latest-visible-files state)))
    (when (or force
              (not (equal (sort (copy-sequence files) #'string<)
                          (sort (copy-sequence (or last '())) #'string<))))
      (setf (amp--project-state-latest-visible-files state) files)
      (amp--project-broadcast-ide
       state
       (amp--wrap-notification
        `((visibleFilesDidChange . ((uris . ,(vconcat files)))))))
      (amp--log 'debug "visible-files"
                "Project %s: visible files changed, count: %d"
                (amp--describe-project root) (length files)))))

(defun amp--broadcast-visible-files (&optional _force)
  "Broadcast visible files for each active project if changed."
  (when (> (hash-table-count amp--projects) 0)
    (maphash
     (lambda (_root state)
       (amp--broadcast-visible-files-for-project state))
     amp--projects)))

;;; Diagnostics

(defun amp--severity-to-protocol (severity)
  "Convert SEVERITY to protocol format."
  (pcase severity
    ('flymake-error "error")
    ('eglot-error "error")
    (':error "error")
    ('flymake-warning "warning")
    ('eglot-warning "warning")
    (':warning "warning")
    ('flymake-note "info")
    ('eglot-note "info")
    (':note "info")
    (_ "info")))

(defun amp--diagnostic-to-protocol (diag)
  "Convert Flymake diagnostic DIAG to protocol format."
  (save-excursion
    (let* ((beg (flymake-diagnostic-beg diag))
           (end (flymake-diagnostic-end diag))
           (beg-line (progn (goto-char beg) (1- (line-number-at-pos))))
           (beg-col (- beg (line-beginning-position)))
           (end-line (progn (goto-char end) (1- (line-number-at-pos))))
           (end-col (- end (line-beginning-position)))
           (line-content (buffer-substring-no-properties
                          (line-beginning-position)
                          (line-end-position))))
      `((range . ((startLine . ,beg-line)
                  (startCharacter . ,beg-col)
                  (endLine . ,end-line)
                  (endCharacter . ,end-col)))
        (severity . ,(amp--severity-to-protocol (flymake-diagnostic-type diag)))
        (description . ,(flymake-diagnostic-text diag))
        (lineContent . ,line-content)
        (startOffset . ,beg-col)
        (endOffset . ,end-col)))))

(defun amp--get-diagnostics (path)
  "Get diagnostics for PATH (file or directory prefix).
Returns an array of entries with uri and diagnostics."
  (let ((abs-path (expand-file-name path))
        (entries-by-uri (make-hash-table :test 'equal)))
    (dolist (buf (buffer-list))
      (when-let ((buf-name (buffer-file-name buf)))
        (let ((abs-buf-name (expand-file-name buf-name)))
          (when (string-prefix-p abs-path abs-buf-name)
            (with-current-buffer buf
              (when (and (featurep 'flymake)
                         (bound-and-true-p flymake-mode))
                (let ((diags (mapcar #'amp--diagnostic-to-protocol
                                     (flymake-diagnostics))))
                  (when diags
                    (let ((uri (concat "file://" abs-buf-name)))
                      (puthash uri
                               `((uri . ,uri)
                                 (diagnostics . ,(vconcat diags)))
                               entries-by-uri))))))))))
    (let ((entries nil))
      (maphash (lambda (_key value)
                 (push value entries))
               entries-by-uri)
      (vconcat entries))))

;;; Hooks

(defun amp--ensure-hooks ()
  "Install global hooks if not already installed."
  (unless amp--hooks-installed
    (add-hook 'post-command-hook #'amp--debounced-selection-update)
    (add-hook 'window-configuration-change-hook #'amp--broadcast-visible-files)
    (add-hook 'buffer-list-update-hook #'amp--broadcast-visible-files)
    (setq amp--hooks-installed t)))

(defun amp--ensure-cleanup-hook ()
  "Install kill-emacs cleanup hook if not already installed."
  (unless amp--cleanup-hook-installed
    (add-hook 'kill-emacs-hook #'amp--cleanup-on-exit)
    (setq amp--cleanup-hook-installed t)))

(defun amp--maybe-remove-hooks ()
  "Remove global hooks if no projects are running."
  (when (and amp--hooks-installed
             (= (hash-table-count amp--projects) 0))
    (remove-hook 'post-command-hook #'amp--debounced-selection-update)
    (remove-hook 'window-configuration-change-hook #'amp--broadcast-visible-files)
    (remove-hook 'buffer-list-update-hook #'amp--broadcast-visible-files)
    (setq amp--hooks-installed nil)))

(defun amp--maybe-remove-cleanup-hook ()
  "Remove kill-emacs cleanup hook if no projects are running."
  (when (and amp--cleanup-hook-installed
             (= (hash-table-count amp--projects) 0))
    (remove-hook 'kill-emacs-hook #'amp--cleanup-on-exit)
    (setq amp--cleanup-hook-installed nil)))

;;; Server Management

(defun amp--cleanup-on-exit ()
  "Clean up all Amp servers on Emacs exit."
  (amp--map-projects
   (lambda (state)
     (amp--remove-lockfile (amp--project-state-port state))
     (when (amp--project-state-server state)
       (websocket-server-close (amp--project-state-server state)))))
  (clrhash amp--projects))

;;;###autoload
(defun amp-start ()
  "Start the Amp WebSocket server for the current project.
If not in a project, starts a server for non-project buffers."
  (interactive)
  (let* ((root (amp--current-project-root))
         (existing (amp--get-project-state root)))
    (when existing
      (user-error "Amp server already running for %s (port %d)"
                  (amp--describe-project root)
                  (amp--project-state-port existing)))

    (let* ((auth-token (amp--generate-auth-token))
           (port (+ 10000 (random 55535)))
           (state (make-amp--project-state
                   :root root
                   :auth-token auth-token
                   :port port
                   :clients nil
                   :connected nil
                   :latest-selection nil
                   :latest-visible-files nil))
           (server nil))

      (condition-case err
          (setq server
                (websocket-server
                 port
                 :host 'local
                 :on-open (lambda (ws) (amp--on-client-connect state ws))
                 :on-message (lambda (ws frame)
                               (amp--handle-websocket-message state ws frame))
                 :on-close (lambda (ws) (amp--on-client-disconnect state ws))
                 :on-error (lambda (_ws type err)
                             (amp--log 'error "server"
                                       "WebSocket error (%s): %s" type err))))
        (error
         (user-error "Failed to start Amp server: %s" (error-message-string err))))

      (setf (amp--project-state-server state) server)

      (condition-case err
          (amp--create-lockfile port auth-token root)
        (error
         (websocket-server-close server)
         (user-error "Failed to create lockfile: %s" (error-message-string err))))

      (amp--put-project-state state)
      (amp--ensure-hooks)
      (amp--ensure-cleanup-hook)

      (run-with-timer 0.2 nil
                      (lambda ()
                        (amp--project-broadcast-ide
                         state
                         (amp--wrap-notification
                          `((pluginMetadata . ((version . "0.1.0"))))))))

      (amp--log 'info "server" "Server started for %s on port %d"
                (amp--describe-project root) port)
      (message "Amp server started for %s on port %d"
               (amp--describe-project root) port))))

;;;###autoload
(defun amp-stop ()
  "Stop the Amp WebSocket server for the current project.
If not in a project, stops the server for non-project buffers."
  (interactive)
  (let* ((root (amp--current-project-root))
         (state (amp--get-project-state root)))
    (unless state
      (user-error "No Amp server running for %s" (amp--describe-project root)))

    (amp--remove-lockfile (amp--project-state-port state))
    (when (amp--project-state-server state)
      (websocket-server-close (amp--project-state-server state)))
    (amp--remove-project-state root)
    (amp--maybe-remove-hooks)
    (amp--maybe-remove-cleanup-hook)

    (amp--log 'info "server" "Server stopped for %s" (amp--describe-project root))
    (message "Amp server stopped for %s" (amp--describe-project root))))

;;;###autoload
(defun amp-stop-all ()
  "Stop all Amp WebSocket servers for all projects."
  (interactive)
  (let ((count (hash-table-count amp--projects)))
    (amp--map-projects
     (lambda (state)
       (amp--remove-lockfile (amp--project-state-port state))
       (when (amp--project-state-server state)
         (websocket-server-close (amp--project-state-server state)))))
    (clrhash amp--projects)
    (amp--maybe-remove-hooks)
    (amp--maybe-remove-cleanup-hook)
    (message "Stopped %d Amp server(s)" count)))

;;;###autoload
(defun amp-status (&optional all)
  "Show status of Amp server for current project.
With prefix argument ALL, show all running servers."
  (interactive "P")
  (if all
      (if (= (hash-table-count amp--projects) 0)
          (message "No Amp servers running")
        (let ((lines nil))
          (maphash (lambda (root state)
                     (push (cons (or root "")
                                 (format "  %s: port %d (%s)"
                                         (amp--describe-project root)
                                         (amp--project-state-port state)
                                         (if (amp--project-state-connected state)
                                             "connected" "waiting")))
                           lines))
                   amp--projects)
          (setq lines (sort lines (lambda (a b) (string< (car a) (car b)))))
          (message "Amp servers:\n%s"
                   (string-join (mapcar #'cdr lines) "\n"))))
    (let* ((root (amp--current-project-root))
           (state (amp--get-project-state root)))
      (if state
          (message "Amp server for %s: port %d (%s)"
                   (amp--describe-project root)
                   (amp--project-state-port state)
                   (if (amp--project-state-connected state)
                       "connected" "waiting"))
        (message "No Amp server running for %s" (amp--describe-project root))))))

;;;###autoload
(defun amp-send-message (message)
  "Send MESSAGE to the Amp agent for the current project."
  (interactive "sMessage: ")
  (let* ((root (amp--current-project-root))
         (state (amp--get-project-state root)))
    (unless state
      (user-error "No Amp server running for %s" (amp--describe-project root)))
    (amp--project-broadcast-ide
     state
     (amp--wrap-notification `((userSentMessage . ((message . ,message))))))
    (amp--log 'debug "message" "Message sent to agent for %s"
              (amp--describe-project root))))

;;;###autoload
(defun amp-send-region (start end)
  "Send region between START and END to Amp agent for the current project."
  (interactive "r")
  (amp-send-message (buffer-substring-no-properties start end)))

;;;###autoload
(defun amp-send-to-prompt (text)
  "Send TEXT to append to the Amp prompt for the current project."
  (interactive "sText: ")
  (let* ((root (amp--current-project-root))
         (state (amp--get-project-state root)))
    (unless state
      (user-error "No Amp server running for %s" (amp--describe-project root)))
    (amp--project-broadcast-ide
     state
     (amp--wrap-notification `((appendToPrompt . ((message . ,text))))))
    (amp--log 'debug "message" "Text appended to prompt for %s"
              (amp--describe-project root))))

;;;###autoload
(define-minor-mode amp-mode
  "Minor mode for Amp IDE integration.
DEPRECATED: Use `amp-start' and `amp-stop' directly instead.
This mode is provided for backwards compatibility but does not
work well with multi-project support."
  :global t
  :group 'amp
  :lighter " Amp"
  (if amp-mode
      (unless (amp--get-project-state (amp--current-project-root))
        (amp-start))
    (when (amp--get-project-state (amp--current-project-root))
      (amp-stop))))

(provide 'amp)
;;; amp.el ends here
