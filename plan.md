# Implementation Plan: Event System Enhancements for amp.el

> Status: Draft v7

## 1. Context <!-- id:context -->

The amp.el package has a minimal internal event system (`amp--on`, `amp--emit`) used for `client-connect` and `client-disconnect` events. External code (e.g., user init files) needs to defer actions until the Amp CLI connects, but the current system:

- Has no way to unregister listeners (`amp--off`)
- Has no one-shot support (`amp--once`)
- Uses private `amp--` prefixed functions, making it fragile for external use

This work enables users to reliably hook into connection lifecycle events.

## 2. Goals and Non-Goals <!-- id:goals -->

### Goals

- Add `amp--off` to unregister listeners
- Add `amp--once` for one-shot listeners that auto-remove
- Expose public hooks (`amp-client-connect-hook`, `amp-client-disconnect-hook`) following Emacs conventions
- Maintain backward compatibility with existing internal usage

### Non-Goals

- Changing the WebSocket server architecture
- Adding new event types beyond connect/disconnect
- Deprecating the internal `amp--on`/`amp--emit` system

## 3. High-Level Approach <!-- id:approach -->

1. Extend the internal event system with `amp--off` and `amp--once`
2. Add public Emacs-style hook variables for connection events
3. Integrate hooks into existing `amp--on-client-connect` and `amp--on-client-disconnect` functions
4. Document the public API in README.org

## 4. Detailed Steps <!-- id:steps -->

### 4.1 Add `amp--off` function <!-- id:step-1 -->

<!-- REVIEW_APPLIED: R1 (oracle) - added guard and documented symbol convention -->

- Add `amp--off` to remove a specific callback from an event's listener list
- Uses `delq` to remove by identity (`eq` comparison)
- EVENT-NAME should be a symbol (convention)

```elisp
(defun amp--off (event-name callback)
  "Remove CALLBACK from EVENT-NAME listeners.
EVENT-NAME should be a symbol. Callbacks are compared by identity (eq)."
  (let ((listeners (gethash event-name amp--event-listeners)))
    (when listeners
      (puthash event-name (delq callback listeners) amp--event-listeners))))
```

### 4.2 Add `amp--once` function <!-- id:step-2 -->

<!-- REVIEW_APPLIED: R2, R6 (oracle) - documented snapshot semantics and identity removal -->

- Wraps callback to auto-remove after first invocation
- Uses closure to capture wrapper reference for removal
- **Note**: Listeners removed during `amp--emit` are removed for *future* emits only; the current emit uses a snapshot of the listener list

```elisp
(defun amp--once (event-name callback)
  "Register CALLBACK for EVENT-NAME, auto-removing after first call.
The callback is removed after it fires once. Note that removal during
an emit only affects future emits, not the current iteration."
  (let ((wrapper nil))
    (setq wrapper
          (lambda (&rest args)
            (amp--off event-name wrapper)
            (apply callback args)))
    (amp--on event-name wrapper)))
```

### 4.3 Add public hook variables and event struct <!-- id:step-3 -->

<!-- REVIEW_APPLIED: R3 (oracle) - clarified struct is public API, documented fields -->
<!-- REVIEW_APPLIED: U2 (user) - rename struct to amp-project-state (public) -->
<!-- REVIEW_APPLIED: U3 (user) - use amp-session-state instead of amp-project-state -->
<!-- REVIEW_APPLIED: U4 (user) - keep session state private, create minimal public event struct -->

- Define `amp-client-connect-hook` and `amp-client-disconnect-hook` as standard Emacs hooks
- Place in configuration section near other `defcustom`/`defvar` declarations
- **API decision**: Create a minimal public `amp-client-connection-event` struct containing only `root`. The internal session state remains private (`amp--session-state`) to avoid exposing `auth-token`, `clients`, `latest-*`, etc.

```elisp
(cl-defstruct amp-client-connection-event
  "Event passed to client connection hooks."
  root)  ; project root (string or nil for non-project buffers)

(defvar amp-client-connect-hook nil
  "Hook run when Amp CLI connects.
Functions are called with one argument: an `amp-client-connection-event'.
Use `amp-client-connection-event-root' to get the project root (may be nil).")

(defvar amp-client-disconnect-hook nil
  "Hook run when Amp CLI disconnects.
Functions are called with one argument: an `amp-client-connection-event'.
Use `amp-client-connection-event-root' to get the project root (may be nil).")
```

### 4.3a Rename `amp--project-state` to `amp--session-state` <!-- id:step-3a -->

<!-- REVIEW_APPLIED: U5 (user) - commit rename independently -->

- Rename the struct definition from `amp--project-state` to `amp--session-state` (still private, double-dash)
- Update all accessor references throughout the codebase (e.g., `amp--project-state-root` → `amp--session-state-root`)
- "Session" better reflects what this tracks; remains internal implementation detail
- **Commit separately**: This rename should be its own commit before the hook implementation for clarity

### 4.4 Integrate hooks into connection handlers <!-- id:step-4 -->

<!-- REVIEW_APPLIED: U1 (user) - added project state structure reference -->
<!-- REVIEW_APPLIED: R4, R5 (oracle) - wrap hooks in condition-case for error safety -->
<!-- REVIEW_APPLIED: U4 (user) - create event from session state, pass event to hooks -->

- Modify `amp--on-client-connect` to create an `amp-client-connection-event` and run `amp-client-connect-hook`
- Modify `amp--on-client-disconnect` to create an event and run `amp-client-disconnect-hook`
- Hooks run **after** internal state updates but **before** initial state broadcast
- **Error handling**: Wrap hook calls in `condition-case` to prevent misbehaving hooks from breaking connection handling

```elisp
;; In amp--on-client-connect, after state updates:
(let ((event (make-amp-client-connection-event
              :root (amp--session-state-root state))))
  (condition-case err
      (run-hook-with-args 'amp-client-connect-hook event)
    (error
     (amp--log 'error "hooks"
               "Error in amp-client-connect-hook: %s"
               (error-message-string err)))))
```

**Event structure** (from `cl-defstruct amp-client-connection-event`):
- `root` — project root path (string or nil for non-project buffers)

Hook functions use `(amp-client-connection-event-root event)` to access the root.

### 4.5 Update README.org with public API documentation <!-- id:step-5 -->

- Add section documenting the hooks
- Provide example usage for deferring context messages

## 5. Risks and Mitigations <!-- id:risks -->

<!-- REVIEW_APPLIED: R5 (oracle) - corrected error handling claim -->

| Risk | Severity | Mitigation |
|------|----------|------------|
| Hook functions throwing errors could break connection handling | Medium | Wrap `run-hook-with-args` in `condition-case` and log errors instead of propagating |
| Identity-based removal in `amp--off` could fail with closures | Low | Document that callers should retain reference to registered callback; removal uses `eq` |

## 6. Validation and Success Criteria <!-- id:validation -->

<!-- REVIEW_APPLIED: R7 (oracle) - expanded edge case coverage -->

### Core functionality
- [ ] `amp--off` successfully removes a registered listener
- [ ] `amp--once` fires exactly once then auto-removes
- [ ] `amp-client-connect-hook` fires when CLI connects
- [ ] `amp-client-disconnect-hook` fires when CLI disconnects
- [ ] Hooks receive `amp-client-connection-event` as argument
- [ ] Existing functionality (selection tracking, diagnostics) unaffected

### Error handling
- [ ] Hook that signals error is logged, doesn't crash server
- [ ] Connection handling continues after hook error
- [ ] Remaining hooks still fire after one hook errors

### Edge cases
- [ ] `amp--off` with unregistered callback: no error
- [ ] `amp--off` with event that has no listeners: no error
- [ ] `amp--once` re-entrancy: nested emit of same event doesn't re-fire once-listener

### Multi-project
- [ ] Hooks fire once per session with distinct event objects
- [ ] Event contains correct `root` for each session
- [ ] Nil-project (`root = nil`) events have nil root, hook still fires

### Manual integration test
- [ ] Register hook, run `amp --ide`, verify hook fires

---

## Review History <!-- id:review-history -->

- Round 1
  - Applied: U1
  - Reviewer: jamesl
  - Notes: Added project state structure reference for clarity

- Round 2
  - Applied: R1, R2, R3, R4, R5, R6, R7
  - Reviewer: oracle
  - Notes: Improved amp--off robustness, documented snapshot semantics, clarified struct is public API, added condition-case error handling, corrected risk table, expanded validation criteria

- Round 3
  - Applied: U2, U3, U4, U5
  - Reviewer: jamesl
  - Notes: Keep session state private (amp--session-state), create minimal amp-client-connection-event with just root for hooks; commit rename separately for clarity
