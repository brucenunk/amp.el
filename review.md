# Oracle Review of plan.md

## TL;DR

The overall design is solid: project-keyed state, manual lifecycle, global hooks with per-project routing, and per-project lockfiles are the right approach and align well with Emacs conventions. I'd green-light this with a few targeted refinements: clarify nil-project semantics vs CLI behavior, centralize hook/kill-emacs management, and cleanly migrate from the old global broadcast/state functions.

---

## Recommended Approach (Simple Path)

Below are the key areas I'd adjust or make explicit before/during implementation.

### A. State & Helper APIs

1. **State struct and registry**  
   The `amp--project-state` + `amp--projects` hash-table design is good and minimal. Keep it as-is, but:

   - Add a tiny helper to iterate projects to avoid repeating `maphash` logic and to make future changes safer:
     ```elisp
     (defun amp--map-projects (fn)
       (maphash (lambda (_root state) (funcall fn state)) amp--projects))
     ```
     Then use it in `amp-stop-all`, `amp--broadcast-visible-files`, `amp--cleanup-on-exit`, etc.

2. **`amp--current-project-root`**  
   The helper is correct and keeps behavior simple. Two small suggestions:
   - Use `abbreviate-file-name` in `amp--describe-project` so messages aren't unwieldy:
     ```elisp
     (defun amp--describe-project (root)
       (or (and root (abbreviate-file-name root))
           "non-project buffers"))
     ```
   - Document explicitly (in docstring) that "nil project" is *one global bucket* across Emacs, not one per directory. This matters for CLI expectations (see section D).

3. **Global vs per-project state**  
   The migration table is good. I'd also explicitly confirm these stay global:
   - `amp--selection-timer` (still one global timer is fine)
   - `amp--event-listeners`, `amp--log-buffer`, `amp--last-error`, `amp-log-level`

   You've already called out the event/log ones; just make sure `amp--selection-timer` is treated intentionally as global, and that `amp--update-selection` always lookups the appropriate `state` itself.

---

### B. WebSocket & Broadcast Plumbing

4. **Broadcast API transition (`amp--broadcast-ide`)**

   Right now, the existing code assumes a single global server with:

   ```elisp
   (defun amp--broadcast-ide (payload)
     ;; sends to amp--clients
   )
   ```

   The plan introduces `amp--project-broadcast-ide state ...` and uses it in examples, but the transition needs to be very clear:

   - **Preferred pattern** (simple and safe):
     ```elisp
     (defun amp--project-broadcast-ide (state payload)
       (dolist (ws (amp--project-state-clients state))
         (amp--send-ide ws payload)))

     ;; Optional helper for "current project"
     (defun amp--broadcast-ide (payload)
       (let* ((root (amp--current-project-root))
              (state (amp--get-project-state root)))
         (unless state
           (user-error "No Amp server running for %s" (amp--describe-project root)))
         (amp--project-broadcast-ide state payload)))
     ```

   - Then audit *all* call sites and ensure:
     - Server-internal broadcast uses `amp--project-broadcast-ide state ...`.
     - User-facing commands (`amp-send-message`, `amp-send-to-prompt`, etc.) either:
       - call `amp--broadcast-ide` (which internally resolves current project), or
       - explicitly resolve `state` and call `amp--project-broadcast-ide`.

   The plan already updates some call sites; the main risk is leaving a stray use of the old, global-only semantics.

5. **WebSocket callbacks and state lifetime**

   The closure approach in `amp-start`:

   ```elisp
   :on-open (lambda (ws) (amp--on-client-connect state ws))
   :on-message (lambda (ws frame) (amp--handle-websocket-message state ws frame))
   :on-close (lambda (ws) (amp--on-client-disconnect state ws))
   ```

   is sound. A couple of considerations:

   - Make sure `amp--handle-websocket-message` remains a thin wrapper that just parses the frame and delegates to a `state`-aware handler (e.g., `amp--handle-ide-request`) that doesn't depend on any global server state.
   - It's fine that `state` persists after `amp-stop` (the struct is still referenced from the closure) because it's no longer in the hash; callbacks will just log and update a dead state, but they won't affect lookups.

6. **`amp--send-initial-state` migration**

   Current code has a global `amp--send-initial-state`. The plan mentions `amp--send-initial-state-for-project state` in the callbacks and "update `amp--send-initial-state` to be project-aware".

   To avoid confusion:

   - Rename the function and pass `state` explicitly:
     ```elisp
     (defun amp--send-initial-state-for-project (state)
       (amp--broadcast-visible-files-for-project state t)
       (amp--update-selection-for-project state))
     ```
   - Update any existing callers (probably only `amp--on-client-connect`) to use the new, explicit API and delete/alias the old name to avoid having two semantics in circulation.

---

### C. Hooks & Lifecycle

7. **Hook installation/removal**

   The `amp--hooks-installed` flag and `amp--ensure-hooks`/`amp--maybe-remove-hooks` pattern is right. Make sure you:

   - **Replace** `amp--setup-hooks` / `amp--remove-hooks` fully, not just add the new ones, and update all call sites (`amp-start`, `amp-stop`, tests) to use the new names.
   - Ensure `amp--maybe-remove-hooks` checks `(hash-table-count amp--projects)` and removes hooks only when it's 0.

8. **`kill-emacs-hook` management**

   In the plan, each `amp-start` does:

   ```elisp
   (add-hook 'kill-emacs-hook #'amp--cleanup-on-exit)
   ```

   With multiple projects this will add duplicates. It's harmless but untidy and can cause redundant cleanup calls.

   I'd mirror the hook-flag pattern:

   ```elisp
   (defvar amp--cleanup-hook-installed nil)

   (defun amp--ensure-cleanup-hook ()
     (unless amp--cleanup-hook-installed
       (add-hook 'kill-emacs-hook #'amp--cleanup-on-exit)
       (setq amp--cleanup-hook-installed t)))

   (defun amp--maybe-remove-cleanup-hook ()
     (when (and amp--cleanup-hook-installed
                (= (hash-table-count amp--projects) 0))
       (remove-hook 'kill-emacs-hook #'amp--cleanup-on-exit)
       (setq amp--cleanup-hook-installed nil)))
   ```

   - Call `amp--ensure-cleanup-hook` in `amp-start` after registering the first project.
   - Call `amp--maybe-remove-cleanup-hook` from `amp-stop`/`amp-stop-all` once all servers are stopped.

   And update `amp--cleanup-on-exit` to iterate over *all* `amp--projects` and close servers/remove lockfiles, not just one global server.

9. **`amp-stop-all`**

   The implementation is fine; just make sure you also:

   - Reset any per-project mutable fields if necessary (most will be GC'd when you `clrhash`, but it's good to close servers explicitly as you already do).
   - Call `amp--maybe-remove-hooks` and `amp--maybe-remove-cleanup-hook` so global state is fully cleaned.

10. **`amp-mode` behavior**

    The plan's "Decide `amp-mode` fate" is important; I'd make a decision up front to avoid awkward UX:

    - **Simplest now**: Deprecate `amp-mode` for starting/stopping servers. Keep it as a cosmetic / opt-in tracking toggle (or remove entirely) and document that `amp-start`/`amp-stop` are the canonical lifecycle commands.
    - If you keep it:
      - Define clearly what it does in a multi-project world. The most predictable behavior is: "when enabled, automatically start a server for the current project if not running; when disabled, stop all servers" – but that reintroduces implicit lifecycle you've explicitly tried to avoid.

    Given your "manual lifecycle only" principle, deprecation or a no-op mode that just affects logging/UX is the least surprising.

---

### D. Lockfile & Workspace Semantics

11. **Per-project `workspaceFolders`**

    Using:

    ```elisp
    (workspaceFolders . [,workspace])
    ```

    where `workspace` is the project-root (normalized) is exactly what's needed for multi-project support.

    The main thing to double-check is **how the CLI chooses** among multiple lockfiles:

    - Typically you want the CLI to pick the server whose `workspaceFolders` entry is the **longest matching prefix** of its working directory.
    - Ensure the CLI already does this (or update it in tandem).

12. **Nil-project semantics vs CLI**

    Plan behavior:

    ```elisp
    (workspace (file-name-as-directory
                (expand-file-name (or root default-directory))))
    ```

    When `root` is nil, this "locks in" whatever `default-directory` happened to be at `amp-start` time. But:

    - All non-project buffers share the same `nil` key in `amp--projects`, regardless of directory.
    - The CLI likely only sees that one `workspaceFolders` entry when deciding which server to attach to.

    This can lead to mismatch:

    - Start nil-project server in `~/scratch`.
    - Later, from CLI in `/tmp`, you might expect it to reuse that nil-project server (since Emacs is tracking your `/tmp` buffer), but the workspace path won't match.

    Two possible minimal adjustments:

    - **Option A (simplest, recommended for now):**
      - Keep behavior as in the plan, but *document* nil-project semantics as "per-directory where you ran `amp-start`", and treat other non-project directories as out of scope until there's a concrete need.
      - This is usually fine for casual use: people start nil-project once per ad-hoc directory.

    - **Option B (slightly more involved, more robust):**
      - Use `/` or `$HOME` as the `workspaceFolders` entry when `root` is nil, and rely on the CLI's "longest prefix wins" logic so real projects override the catch-all root.
      - This makes the nil-project server available "everywhere" but requires that the CLI properly prioritizes more specific project roots over `/`.

    Either way, coordinating with the CLI team (or tests) to lock in the matching semantics is important; the Emacs side alone can't guarantee this.

---

### E. Selection & Visible-Files Tracking

13. **`amp--get-visible-files-for-project` & `amp--broadcast-visible-files`**

    The proposed logic is correct:

    - Iterate windows → visible buffers → filter by project-root (`equal` handles nil) → deduplicate paths per project → compare sorted lists to detect changes.

    Minor improvements:

    - Factor out the sorting/comparison into a helper to avoid repeating the `sort` boilerplate.
    - Confirm that `amp--current-project-root` is cheap enough to call per buffer. For typical P (projects) and W (windows), this is O(P×W), but with small P and W this is fine.

14. **Selection routing**

    The outlined pattern:

    ```elisp
    (let* ((root (amp--current-project-root))
           (state (amp--get-project-state root)))
      (when state
        ;; broadcast to this project's server
        ))
    ```

    is exactly what you want. Ensure `amp--debounced-selection-update` just calls into an updated `amp--update-selection` that:

    - Determines `state` from the *current buffer*.
    - Does nothing if there's no server for that project (no global errors).

---

### F. Commands & UX

15. **`amp-status`**

    The new `amp-status` with the optional prefix argument is a nice improvement. Two details:

    - For the "all" case, consider ordering the output alphabetically by root (instead of hash-table iteration order) for predictability.
    - In the single-project case, if you hit it in a buffer without any server (including nil-project with no server), the message "No Amp server running for non-project buffers" is clear; that's good.

16. **`amp-send-message` / `amp-send-to-prompt` / `amp-send-region`**

    The project-aware versions are fine. Just be consistent:

    - Always resolve `state` from `amp--current-project-root` and then call `amp--project-broadcast-ide`.
    - Use `amp--describe-project` in logs so it's easier to debug routing issues when multiple projects are active.

---

## Rationale and Trade-offs

- **Project-keyed state + global hooks** is the minimal change from the current single-server architecture that gives you multi-project support without introducing complex lifecycle automation. It reuses existing patterns and keeps the mental model simple.
- Routing everything through `state` captured in callbacks and used by new helpers (`amp--project-broadcast-ide`, `amp--get-visible-files-for-project`) avoids subtle global-state bugs and is easier to reason about.
- Not touching the CLI behavior beyond `workspaceFolders` keeps the Emacs changes focused. Where ambiguity remains (nil-project semantics), you can resolve it mostly via documentation plus a small CLI tweak if needed later.

---

## Risks and Guardrails

- **CLI/server mismatch for nil-project**: The main functional risk is unexpected behavior when using Emacs' nil-project server from directories different than where it was started. Mitigation: clarify semantics in docs; add integration tests with CLI once released; adjust nil-project `workspaceFolders` if needed.
- **Half-migrated state**: Leaving old globals (`amp--server`, old `amp--broadcast-ide`, `amp--setup-hooks`) partially used will cause hard-to-diagnose bugs. Mitigation: Phase 6 "Cleanup" should explicitly remove/alias those symbols and run a quick grep sanity check.
- **Multiple cleanup hooks**: Without centralized `kill-emacs-hook` management, you'll get redundant cleanup calls. Not fatal, but can mask subtle issues; mitigate via the hook flag pattern above.
- **Performance**: For very large numbers of projects/windows, selection and visible-files recomputation cost could rise, but with typical usage this is negligible. If it ever becomes an issue, you can optimize via caching or per-window project tagging.

---

## When to Consider the Advanced Path

Revisit the design (and consider more advanced behavior) if:

- Users frequently want **automatic** project start/stop behavior instead of manual `amp-start`/`amp-stop`.
- You see significant **performance issues** from per-project visible-files/selection tracking with many projects and windows.
- There's a strong requirement to handle **many concurrent non-project workspaces** more precisely than the single nil-project bucket allows.

---

## Optional Advanced Path (Outline Only)

If/when needed:

- Introduce `amp-project-auto-mode` as you sketched, but make it:
  - Track last-used time per `amp--project-state` and stop idle projects automatically.
  - Hook into `project-switch-project-hook` and maybe `find-file-hook` to lazily start servers on first relevant buffer.
- Enhance lockfile semantics to include additional metadata (e.g., project name, hash of root) for better CLI UX and debugging.
- Provide a small UI widget (e.g., `amp-projects-status` buffer) listing all known projects, their ports, and connection statuses, with actions bound to keys.

---

## Effort/Scope Assessment

Your phase breakdown (~4.5–6.5 hours) looks reasonable. With the refinements above (centralized cleanup hooks, a bit of extra audit work, and nil-project documentation), I'd still treat this as an **L (~1 working day)** task including tests and docs.
