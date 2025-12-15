# Project-Based Multi-Project Support Plan for amp.el

## Executive Summary

Use `project.el` to detect the current project root and maintain a hash table `root → project-state`. Commands `amp-start`/`amp-stop` become project-aware and remain strictly manual (no automatic lifecycle hooks in core). Each project server gets its own lockfile with `workspaceFolders` set correctly.

---

## Design Overview

### Core Principles

1. **Project-keyed state** - One Amp server per `project-root`, stored in a hash table
2. **Manual lifecycle only** - No auto-start/stop hooks in core; `amp-start`/`amp-stop` are explicit
3. **Global hooks, per-project routing** - Selection and visible-files hooks remain global but route updates to the correct project's server
4. **Non-project buffer support** - Buffers without a project belong to a special `nil` project, allowing Amp to work outside of project.el projects

---

## 1. State Storage

### 1.1 Project State Structure

```elisp
(require 'project)

(cl-defstruct amp--project-state
  root                    ; string (normalized project root) or nil for non-project
  server                  ; websocket server object
  port                    ; integer
  auth-token              ; string
  clients                 ; list of client websockets
  connected               ; boolean
  latest-selection        ; last selection payload sent
  latest-visible-files)   ; last visible-files list
```

**Note:** The `root` field can be `nil` for non-project buffers. The hash table uses `:test #'equal` which correctly handles `nil` as a key.

### 1.2 Project Registry

```elisp
(defvar amp--projects (make-hash-table :test #'equal)
  "Map project root (string) to `amp--project-state`.")

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
```

### 1.3 Current Project Helpers

```elisp
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
```

**Note:** We no longer have `amp--current-project-root-or-error` since `nil` is a valid project key representing all non-project buffers.

**Nil-project semantics:** The `nil` project represents a *single global bucket* for all non-project buffers across Emacs. When `amp-start` is run for a nil-project, the lockfile's `workspaceFolders` is set to the `default-directory` at that moment. This means CLI invocations from other non-project directories may not match this server. This is acceptable for casual use where users typically start one nil-project server per ad-hoc directory.

### 1.4 Variables to Migrate

| Current Global | New Location |
|---------------|--------------|
| `amp--server` | `(amp--project-state-server state)` |
| `amp--port` | `(amp--project-state-port state)` |
| `amp--auth-token` | `(amp--project-state-auth-token state)` |
| `amp--clients` | `(amp--project-state-clients state)` |
| `amp--connected` | `(amp--project-state-connected state)` |
| `amp--latest-selection` | `(amp--project-state-latest-selection state)` |
| `amp--latest-visible-files` | `(amp--project-state-latest-visible-files state)` |

**Remain global:** `amp--event-listeners`, `amp--log-buffer`, `amp--last-error`, `amp-log-level`, `amp--selection-timer`

**Note:** `amp--selection-timer` remains global (one timer for all projects). The timer's callback (`amp--update-selection`) resolves the appropriate project state from the current buffer.

---

## 2. Project Detection

### 2.1 Command Context

All project-aware commands resolve the project root from `project-current` (which may return `nil` for non-project buffers):

```elisp
;; Pattern used in amp-start, amp-stop, amp-status, amp-send-message, etc.
(let* ((root (amp--current-project-root))  ; nil is valid
       (state (amp--get-project-state root)))
  ...)
```

### 2.2 Buffer Context for Tracking

Selection and visible-files tracking use `amp--current-project-root` per-buffer. Since `nil` is valid, we use `amp--get-project-state` directly:

```elisp
;; In amp--update-selection
(let* ((root (amp--current-project-root))
       (state (amp--get-project-state root)))
  (when state  ; only broadcast if server is running for this project/nil
    ;; broadcast to this project's server
    ))
```

### 2.3 Root Normalization

Always use `expand-file-name` to normalize roots for consistent hash keys:

```elisp
(expand-file-name (project-root proj))
```

---

## 3. Lifecycle Management (Manual)

### 3.1 Project-Aware `amp-start`

```elisp
(defun amp-start ()
  "Start the Amp WebSocket server for the current project.
If not in a project, starts a server for non-project buffers."
  (interactive)
  (let* ((root (amp--current-project-root))  ; nil is valid
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
      
      ;; Create WebSocket server with closures capturing state
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
      
      ;; Create lockfile with workspaceFolders set to project root
      (condition-case err
          (amp--create-lockfile port auth-token root)
        (error
         (websocket-server-close server)
         (user-error "Failed to create lockfile: %s" (error-message-string err))))
      
      ;; Register state and ensure hooks
      (amp--put-project-state state)
      (amp--ensure-hooks)
      (amp--ensure-cleanup-hook)
      
      ;; Send plugin metadata
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
```

### 3.2 Project-Aware `amp-stop`

```elisp
(defun amp-stop ()
  "Stop the Amp WebSocket server for the current project.
If not in a project, stops the server for non-project buffers."
  (interactive)
  (let* ((root (amp--current-project-root))  ; nil is valid
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
```

### 3.3 Stop All Servers

```elisp
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
```

### 3.4 Global Hook Management

```elisp
(defvar amp--hooks-installed nil
  "Whether global tracking hooks are installed.")

(defvar amp--cleanup-hook-installed nil
  "Whether the kill-emacs cleanup hook is installed.")

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
```

### 3.5 Cleanup on Emacs Exit

```elisp
(defun amp--cleanup-on-exit ()
  "Clean up all Amp servers on Emacs exit."
  (amp--map-projects
   (lambda (state)
     (amp--remove-lockfile (amp--project-state-port state))
     (when (amp--project-state-server state)
       (websocket-server-close (amp--project-state-server state)))))
  (clrhash amp--projects))
```

---

## 4. Edge Cases

### 4.1 Non-Project Buffers (nil Project)

Non-project buffers are fully supported via a special `nil` project key:

| Scenario | Behavior |
|----------|----------|
| `amp-start` in non-project buffer | Starts server for "non-project buffers" |
| `amp-stop` in non-project buffer | Stops the non-project server |
| Selection in non-project buffer | Broadcast to nil-project server (if running) |
| Visible files from non-project buffer | Included in nil-project's `visibleFilesDidChange` |

### 4.2 Scratch Buffers and Special Buffers

- `*scratch*`, `*Messages*`, etc. typically have no `buffer-file-name`
- Already ignored by selection tracking (no file URI to report)
- Non-file buffers don't contribute to visible files
- But if cursor is in such a buffer, no selection update is sent (expected behavior)

### 4.3 Mixed Projects in One Frame

When computing visible files:
- Filter by project root (including `nil` for non-project files)
- Windows with project-A files → A's `visibleFilesDidChange`
- Windows with project-B files → B's `visibleFilesDidChange`
- Windows with non-project files → nil-project's `visibleFilesDidChange`

Selection updates:
- Follow the current buffer's project only
- Cursor in project-A buffer → update A's server only
- Cursor in non-project buffer → update nil-project server only

### 4.4 Project Hierarchy

If a file is in a subdirectory that could match multiple projects, `project-current` determines the project. This is standard project.el behavior and amp.el follows it.

---

## 5. Per-Project Tracking

### 5.1 Project Broadcast Primitives

```elisp
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
    (amp--project-broadcast-ide state payload)))
```

**Usage:** Server-internal broadcasts should use `amp--project-broadcast-ide` with an explicit `state`. User-facing commands can use `amp--broadcast-ide` which resolves the current project automatically.

### 5.2 Selection Updates

```elisp
(defun amp--update-selection ()
  "Update and broadcast selection for the current buffer's project.
Works for both project buffers and non-project buffers (nil project)."
  (let* ((root (amp--current-project-root))  ; nil is valid
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
```

### 5.3 Visible Files Per Project

```elisp
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
              ;; Use equal to correctly compare nil with nil
              (when (equal buf-root root)
                (let ((abs (expand-file-name file)))
                  (unless (gethash abs seen)
                    (puthash abs t seen)
                    (push (concat "file://" abs) uris)))))))))
    (nreverse uris)))

(defun amp--broadcast-visible-files (&optional _force)
  "Broadcast visible files for each active project if changed."
  (when (> (hash-table-count amp--projects) 0)
    (maphash
     (lambda (_root state)
       (let* ((root (amp--project-state-root state))
              (files (amp--get-visible-files-for-project root))
              (last (amp--project-state-latest-visible-files state)))
         (unless (equal (sort (copy-sequence files) #'string<)
                        (sort (copy-sequence (or last '())) #'string<))
           (setf (amp--project-state-latest-visible-files state) files)
           (amp--project-broadcast-ide
            state
            (amp--wrap-notification
             `((visibleFilesDidChange . ((uris . ,(vconcat files)))))))
           (amp--log 'debug "visible-files"
                     "Project %s: visible files changed, count: %d"
                     (amp--describe-project root) (length files)))))
     amp--projects)))
```

---

## 6. Lockfile Strategy

### 6.1 Per-Project Lockfile

```elisp
(defun amp--create-lockfile (port auth-token root)
  "Create a lockfile for PORT with AUTH-TOKEN and project ROOT.
ROOT may be nil for non-project buffers, in which case default-directory is used."
  (let* ((lock-dir (amp--lock-dir))
         (lockfile-path (expand-file-name (format "%d.json" port) lock-dir))
         ;; For nil project, use current working directory; otherwise use project root
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
```

**Key changes:**
- `workspaceFolders` is set to `[project-root]` for project buffers
- `workspaceFolders` is set to `[default-directory]` for non-project buffers (nil project)
- This allows the Amp CLI to match the correct server based on working directory

**CLI matching note:** The CLI typically picks the server whose `workspaceFolders` entry is the longest matching prefix of its working directory. For nil-project servers, the `workspaceFolders` is locked to the `default-directory` at `amp-start` time. CLI invocations from other directories may not match this server. Coordinate with CLI behavior to ensure proper matching semantics.

---

## 7. WebSocket Callbacks

Server callbacks need closures capturing the project state:

```elisp
(defun amp--on-client-connect (state ws)
  "Handle client connection for project STATE from WS."
  (push ws (amp--project-state-clients state))
  (setf (amp--project-state-connected state) t)
  (amp--log 'info "server" "Client connected to project %s"
            (amp--describe-project (amp--project-state-root state)))
  (amp--emit 'client-connect ws)
  ;; Send initial state for this project
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

(defun amp--handle-websocket-message (state ws frame)
  "Handle incoming WebSocket message for project STATE from WS with FRAME."
  ;; ... existing logic, but responses go to ws, broadcasts use state
  )
```

---

## 8. Command Updates

### 8.1 `amp-status`

```elisp
(defun amp-status (&optional all)
  "Show status of Amp server for current project.
With prefix argument ALL, show all running servers."
  (interactive "P")
  (if all
      ;; Show all running servers
      (if (= (hash-table-count amp--projects) 0)
          (message "No Amp servers running")
        (let ((lines nil))
          (maphash (lambda (root state)
                     (push (cons (or root "")  ; sort key
                                 (format "  %s: port %d (%s)"
                                         (amp--describe-project root)
                                         (amp--project-state-port state)
                                         (if (amp--project-state-connected state)
                                             "connected" "waiting")))
                           lines))
                   amp--projects)
          ;; Sort alphabetically by root for predictable output
          (setq lines (sort lines (lambda (a b) (string< (car a) (car b)))))
          (message "Amp servers:\n%s"
                   (string-join (mapcar #'cdr lines) "\n"))))
    ;; Show current project only
    (let* ((root (amp--current-project-root))  ; nil is valid
           (state (amp--get-project-state root)))
      (if state
          (message "Amp server for %s: port %d (%s)"
                   (amp--describe-project root)
                   (amp--project-state-port state)
                   (if (amp--project-state-connected state)
                       "connected" "waiting"))
        (message "No Amp server running for %s" (amp--describe-project root))))))
```

### 8.2 `amp-send-message` / `amp-send-to-prompt`

```elisp
(defun amp-send-message (message)
  "Send MESSAGE to the Amp agent for the current project."
  (interactive "sMessage: ")
  (let* ((root (amp--current-project-root))  ; nil is valid
         (state (amp--get-project-state root)))
    (unless state
      (user-error "No Amp server running for %s" (amp--describe-project root)))
    (amp--project-broadcast-ide
     state
     (amp--wrap-notification `((userSentMessage . ((message . ,message))))))
    (amp--log 'debug "message" "Message sent to agent for %s"
              (amp--describe-project root))))
```

---

## 9. Implementation Checklist

### Phase 0: Preparation
- [ ] Add `(require 'project)` at top
- [ ] Verify Emacs 28+ requirement (project.el built-in)
- [ ] Introduce `amp--current-project-root` helper

### Phase 1: State Infrastructure
- [ ] Define `amp--project-state` struct
- [ ] Create `amp--projects` hash table
- [ ] Add `amp--get-project-state`, `amp--put-project-state`, `amp--remove-project-state`
- [ ] Add `amp--project-broadcast-ide`

### Phase 2: Lifecycle Commands
- [ ] Rewrite `amp-start` to be project-aware
- [ ] Rewrite `amp-stop` to be project-aware
- [ ] Add `amp-stop-all` command
- [ ] Implement `amp--ensure-hooks` / `amp--maybe-remove-hooks`
- [ ] Update `amp--cleanup-on-exit` for all projects

### Phase 3: WebSocket Callbacks
- [ ] Update `amp--on-client-connect` to accept state
- [ ] Update `amp--on-client-disconnect` to accept state
- [ ] Update `amp--handle-websocket-message` to accept state
- [ ] Update `amp--send-initial-state` to be project-aware

### Phase 4: Per-Project Tracking
- [ ] Update `amp--update-selection` for project routing
- [ ] Create `amp--get-visible-files-for-project`
- [ ] Update `amp--broadcast-visible-files` to iterate projects
- [ ] Update lockfile creation with `workspaceFolders`

### Phase 5: Command Updates
- [ ] Update `amp-status` (with optional prefix for all)
- [ ] Update `amp-send-message` for project context
- [ ] Update `amp-send-to-prompt` for project context
- [ ] Update `amp-send-region` for project context

### Phase 6: Cleanup
- [ ] Remove obsolete global variables (`amp--server`, `amp--port`, `amp--auth-token`, `amp--clients`, `amp--connected`, `amp--latest-selection`, `amp--latest-visible-files`)
- [ ] Deprecate `amp-mode` - recommend `amp-start`/`amp-stop` as canonical lifecycle commands
- [ ] Update documentation

**Note on `amp-mode`:** Given the "manual lifecycle only" principle, `amp-mode` should be deprecated. The most predictable behavior in a multi-project world is to use explicit `amp-start`/`amp-stop` commands. Keeping `amp-mode` would either need to auto-start servers (violating the manual principle) or become a no-op toggle affecting only logging/UX.

### Testing
- [ ] Test: start server for project A
- [ ] Test: start server for project B (different root)
- [ ] Test: start server for non-project buffers (nil project)
- [ ] Test: selection updates route to correct server
- [ ] Test: selection in non-project buffer routes to nil-project server
- [ ] Test: visible files filtered correctly per project
- [ ] Test: non-project files included in nil-project's visible files
- [ ] Test: `amp-stop` only affects current project
- [ ] Test: `amp-stop` in non-project buffer stops nil-project server
- [ ] Test: `amp-stop-all` cleans up everything (including nil-project)
- [ ] Test: mixed projects in one frame routes correctly
- [ ] Test: Emacs exit cleans up all servers

---

## Estimated Effort

| Phase | Effort |
|-------|--------|
| Phase 0-1: State infrastructure | 30-45 min |
| Phase 2: Lifecycle commands | 45-60 min |
| Phase 3: WebSocket callbacks | 30-45 min |
| Phase 4: Per-project tracking | 45-60 min |
| Phase 5: Command updates | 20-30 min |
| Phase 6: Cleanup | 15-20 min |
| Testing | 60-90 min |
| **Total** | **4.5-6.5 hours** |

*Note: nil-project support adds minimal overhead (~30 min) as it's mostly replacing `string-equal` with `equal` and using `amp--describe-project` for messages.*

---

## Optional: Future Enhancements (Out of Scope for Core)

If automatic lifecycle is desired later, create a separate optional mode:

```elisp
(define-minor-mode amp-project-auto-mode
  "Auto-start/stop Amp servers based on project activity."
  :global t
  (if amp-project-auto-mode
      (progn
        (add-hook 'project-switch-project-hook #'amp--auto-start-project)
        (add-hook 'buffer-list-update-hook #'amp--auto-stop-idle-projects))
    (remove-hook 'project-switch-project-hook #'amp--auto-start-project)
    (remove-hook 'buffer-list-update-hook #'amp--auto-stop-idle-projects)))
```

This keeps the core simple while allowing opt-in automation.
