# Review of plan.md v2

## Summary

- Overall assessment: Solid plan with good Emacs idioms; needs fixes for error handling and API clarity
- Review focus: correctness | API design | risk

## Global Comments

- The event system extensions are well-designed and idiomatic for Emacs Lisp
- Main concerns: `run-hook-with-args` doesn't catch errors (contrary to plan's claim), and passing `amp--project-state` blurs public/private API boundaries

## Section-Specific Feedback

- [R1] (section: step-1, target_id: step-1, severity: low)
  - Issue: Minor robustness improvements possible for `amp--off`
  - Recommendation: Add `when listeners` guard and document that EVENT-NAME should be a symbol

- [R2] (section: step-2, target_id: step-2, severity: low)
  - Issue: Implementation is correct but lacks clarity on emit-during-iteration semantics
  - Recommendation: Add comment above `amp--emit` clarifying snapshot behavior

- [R3] (section: step-3, target_id: step-3, severity: medium)
  - Issue: Passing `amp--project-state` struct to public hooks exposes private internals
  - Recommendation: Either rename struct to `amp-project-state` (public) or pass a stable alist/plist instead

- [R4] (section: step-4, target_id: step-4, severity: low)
  - Issue: Hook argument design is good; just needs struct visibility decision
  - Recommendation: Keep the design, adjust per R3

- [R5] (section: risks, target_id: risks, severity: medium)
  - Issue: `run-hook-with-args` does NOT handle errors gracefully—it propagates them
  - Recommendation: Wrap in `condition-case` and log errors instead

- [R6] (section: step-2, target_id: step-2, severity: low)
  - Issue: One-shot removal semantics need documentation
  - Recommendation: Document that `amp--off` uses identity comparison (`eq`)

- [R7] (section: validation, target_id: validation, severity: medium)
  - Issue: Missing edge case tests for error handling, multi-project, nil-project, amp--off edge cases
  - Recommendation: Expand validation checklist with these scenarios

- [R8] (section: approach, target_id: approach, severity: info)
  - Issue: None—feasibility is good
  - Recommendation: Proceed after addressing R3 and R5; effort estimate S (1-2 hours)
